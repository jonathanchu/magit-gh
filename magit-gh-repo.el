;;; magit-gh-repo.el --- Repository commands for magit-gh -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/magit-gh

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Repository (`gh repo') commands for magit-gh:
;;
;;   - View information about a repository in a dedicated buffer
;;   - List an owner's repositories
;;   - Open a repository in the browser
;;   - Fork a repository
;;   - Create a new repository
;;   - Sync a fork with its upstream
;;   - Set the default repository for `gh' commands
;;
;; These commands are reached from the `magit-gh' transient menu and
;; are loaded on demand.  Commands that can act on a repository other
;; than the current one accept an optional `[owner/]repo' argument when
;; invoked with a prefix argument (or, where shown, via completion).

;;; Code:

(require 'magit)
(require 'magit-gh-utils)

;;; Custom Variables

(defcustom magit-gh-repo-limit 30
  "Maximum number of repositories to fetch when listing."
  :type 'integer
  :group 'magit-gh)

;;; Helper functions

(defun magit-gh--unnull (value)
  "Return VALUE, or nil when VALUE is the `:null' JSON sentinel.
`magit-gh--async-fetch' parses JSON `null' as the keyword `:null';
this coerces it back to nil for fields that may be absent."
  (unless (eq value :null) value))

(defun magit-gh--read-repo-target (&optional prompt)
  "Read an optional [OWNER/]REPO target and return it.
Return nil when the user enters an empty string, meaning the
current repository.  PROMPT defaults to a generic repository prompt."
  (let ((input (string-trim
                (read-string (or prompt "Repository (owner/repo): ")))))
    (unless (string-empty-p input) input)))

(defun magit-gh--repo-target-arg (target)
  "Return TARGET shell-quoted with a leading space, or an empty string.
Used to append an optional repository argument to a gh command."
  (if (and target (not (string-empty-p target)))
      (concat " " (shell-quote-argument target))
    ""))

(defun magit-gh--current-repo-owner ()
  "Return the owner login of the current repository, or nil."
  (let* ((default-directory (magit-gh--repo-dir))
         (output (string-trim
                  (shell-command-to-string
                   "gh repo view --json owner --jq '.owner.login'"))))
    (unless (string-empty-p output) output)))

(defun magit-gh--remote-nwo (url)
  "Extract an OWNER/REPO string from a GitHub remote URL, or nil."
  (when (and url
             (string-match
              "github\\.com[:/]\\([^/]+\\)/\\(.+?\\)\\(?:\\.git\\)?/?\\'" url))
    (format "%s/%s" (match-string 1 url) (match-string 2 url))))

(defun magit-gh--remote-repo-candidates ()
  "Return a list of OWNER/REPO strings derived from the repo's remotes."
  (let ((default-directory (magit-gh--repo-dir)))
    (delete-dups
     (delq nil
           (mapcar (lambda (remote)
                     (magit-gh--remote-nwo
                      (magit-git-string "remote" "get-url" remote)))
                   (magit-list-remotes))))))

(defun magit-gh--repo-and-parent ()
  "Return OWNER/REPO for the current repository and its parent.
Returns a list whose first element is the resolved repository and
whose second is its fork parent (when the repository is a fork).
Either element may be absent.  Uses a synchronous gh call."
  (let* ((default-directory (magit-gh--repo-dir))
         (output (string-trim
                  (shell-command-to-string
                   "gh repo view --json nameWithOwner,parent"))))
    (when (string-prefix-p "{" output)
      (let* ((data (json-parse-string output
                                      :array-type 'list :object-type 'alist))
             (nwo (magit-gh--unnull (alist-get 'nameWithOwner data)))
             (parent (magit-gh--unnull (alist-get 'parent data)))
             (powner (alist-get 'login (alist-get 'owner parent)))
             (pname (alist-get 'name parent))
             (parent-nwo (and powner pname (format "%s/%s" powner pname))))
        (delq nil (list nwo parent-nwo))))))

(defun magit-gh--default-repo-candidates ()
  "Return candidate OWNER/REPO strings for setting the default repository.
Combines the current repository and its fork parent with the
repository's GitHub remotes, mirroring the choices that
`gh repo set-default' offers.  The fork parent, when present, is
listed first so it serves as the default choice, since the
upstream is the usual target for `gh' commands."
  (let* ((pair (magit-gh--repo-and-parent))
         (repo (car pair))
         (parent (cadr pair)))
    (delete-dups
     (delq nil (append (list parent repo)
                       (magit-gh--remote-repo-candidates))))))

(defun magit-gh--run-reporting (cmd success failure)
  "Run shell CMD in `default-directory', reporting the result.
On a zero exit code show SUCCESS as a message; otherwise signal
FAILURE as a `user-error'.  Command output is collected in the
*magit-gh-output* buffer."
  (let ((buf (get-buffer-create "*magit-gh-output*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (if (= (call-process-shell-command cmd nil buf nil) 0)
        (message "%s" success)
      (user-error "%s; see *magit-gh-output* buffer" failure))))

;;; Repo Info Buffer Mode

(defvar-local magit-gh-repo-view--repo-dir nil
  "The repository directory for the current repo info buffer.")

(defvar-local magit-gh-repo-view--target nil
  "The [owner/]repo target shown in the current repo info buffer.
Nil means the current repository.")

(defvar magit-gh-repo-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "v") #'magit-gh-repo-view-browse)
    (define-key map (kbd "o") #'magit-gh-repo-view-browse)
    (define-key map (kbd "g") #'magit-gh-repo-view-refresh)
    map)
  "Keymap for `magit-gh-repo-view-mode'.")

(define-derived-mode magit-gh-repo-view-mode special-mode "GH-Repo"
  "Major mode for viewing GitHub repository information.

\\<magit-gh-repo-view-mode-map>\
\\[magit-gh-repo-view-browse] - Open the repository in the browser
\\[magit-gh-repo-view-refresh] - Refresh the repository info
\\[quit-window] - Close the buffer"
  :group 'magit-gh
  (setq-local header-line-format " v/o:browse  g:refresh  q:quit"))

;;; Repo Info Rendering

(defun magit-gh-repo-view--insert-field (label value)
  "Insert a LABEL: VALUE line when VALUE is a non-empty string."
  (when (and value (not (string-empty-p value)))
    (insert (propertize (format "%-16s" (concat label ":"))
                        'face 'magit-gh-pr-author)
            value "\n")))

(defun magit-gh-repo-view--topics (topics)
  "Return a comma-separated string of TOPICS, or nil.
TOPICS may be a list of strings or of alists with a `name' key."
  (when (and topics (listp topics))
    (mapconcat (lambda (tp)
                 (if (stringp tp) tp (or (alist-get 'name tp) "")))
               topics ", ")))

(defun magit-gh-repo-view--render (buf data)
  "Render repository DATA into BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (nwo (magit-gh--unnull (alist-get 'nameWithOwner data)))
             (desc (magit-gh--unnull (alist-get 'description data)))
             (branch (alist-get 'name (magit-gh--unnull
                                       (alist-get 'defaultBranchRef data))))
             (stars (alist-get 'stargazerCount data))
             (forks (alist-get 'forkCount data))
             (private (alist-get 'isPrivate data))
             (is-fork (alist-get 'isFork data))
             (parent (magit-gh--unnull (alist-get 'parent data)))
             (topics (magit-gh--unnull (alist-get 'repositoryTopics data)))
             (url (magit-gh--unnull (alist-get 'url data)))
             (ssh (magit-gh--unnull (alist-get 'sshUrl data)))
             (updated (magit-gh--unnull (alist-get 'updatedAt data))))
        (erase-buffer)
        (insert (propertize (or nwo "Repository") 'face 'magit-gh-header) "\n")
        (when (and desc (not (string-empty-p desc)))
          (insert (propertize desc 'face 'magit-gh-pr-title) "\n"))
        (insert "\n")
        (magit-gh-repo-view--insert-field
         "Visibility" (if (eq private t) "private" "public"))
        (magit-gh-repo-view--insert-field "Default branch" branch)
        (magit-gh-repo-view--insert-field
         "Stars" (and (numberp stars) (number-to-string stars)))
        (magit-gh-repo-view--insert-field
         "Forks" (and (numberp forks) (number-to-string forks)))
        (when (eq is-fork t)
          (magit-gh-repo-view--insert-field
           "Forked from"
           (let ((powner (alist-get 'login (alist-get 'owner parent)))
                 (pname (alist-get 'name parent)))
             (when (and powner pname) (format "%s/%s" powner pname)))))
        (magit-gh-repo-view--insert-field
         "Topics" (magit-gh-repo-view--topics topics))
        (magit-gh-repo-view--insert-field "Updated" (magit-gh--format-age updated))
        (magit-gh-repo-view--insert-field "URL" url)
        (magit-gh-repo-view--insert-field "SSH" ssh)
        (goto-char (point-min))))))

;;; Repo Info Commands

;;;###autoload
(defun magit-gh-repo-view (&optional target)
  "Show information about a GitHub repository in a dedicated buffer.
With a prefix argument, prompt for a [OWNER/]REPO TARGET; otherwise
show the current repository."
  (interactive (list (when current-prefix-arg (magit-gh--read-repo-target))))
  (magit-gh--check-gh)
  (let* ((repo-dir (magit-gh--repo-dir))
         (default-directory repo-dir)
         (cmd (concat "gh repo view"
                      (magit-gh--repo-target-arg target)
                      " --json nameWithOwner,description,defaultBranchRef,"
                      "stargazerCount,forkCount,isPrivate,isFork,parent,"
                      "repositoryTopics,url,sshUrl,updatedAt"))
         (buf (get-buffer-create "*magit-gh: Repository*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Loading..." 'face 'magit-gh-pr-author)))
      (magit-gh-repo-view-mode)
      (setq magit-gh-repo-view--repo-dir repo-dir)
      (setq magit-gh-repo-view--target target))
    (pop-to-buffer buf)
    (magit-gh--async-fetch
     cmd
     (lambda (data) (magit-gh-repo-view--render buf data))
     (lambda (msg)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (propertize msg
                                 'face 'magit-gh-pr-review-changes-requested)))))))))

;;;###autoload
(defun magit-gh-repo-browse (&optional target)
  "Open a GitHub repository in the browser.
With a prefix argument, prompt for a [OWNER/]REPO TARGET; otherwise
open the current repository."
  (interactive (list (when current-prefix-arg (magit-gh--read-repo-target))))
  (magit-gh--check-gh)
  (let ((default-directory (magit-gh--repo-dir)))
    (shell-command (concat "gh repo view"
                           (magit-gh--repo-target-arg target)
                           " --web"))))

(defun magit-gh-repo-view-browse ()
  "Open the repository shown in the current buffer in the browser."
  (interactive)
  (let ((default-directory magit-gh-repo-view--repo-dir))
    (magit-gh-repo-browse magit-gh-repo-view--target)))

(defun magit-gh-repo-view-refresh ()
  "Refresh the repository info buffer."
  (interactive)
  (let ((default-directory magit-gh-repo-view--repo-dir))
    (magit-gh-repo-view magit-gh-repo-view--target)))

;;; Repo List Buffer Mode

(defvar-local magit-gh-repo-list--repo-dir nil
  "The repository directory for the current repo list buffer.")

(defvar-local magit-gh-repo-list--owner nil
  "The owner whose repositories are shown in the current list buffer.")

(defvar-local magit-gh-repo-list--args nil
  "The `gh repo list' filter arguments used for the current list buffer.")

(defvar magit-gh-repo-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'magit-gh-repo-list-view)
    (define-key map (kbd "v") #'magit-gh-repo-list-browse)
    (define-key map (kbd "o") #'magit-gh-repo-list-browse)
    (define-key map (kbd "n") #'magit-gh--next-item)
    (define-key map (kbd "p") #'magit-gh--previous-item)
    (define-key map (kbd "g") #'magit-gh-repo-list-refresh)
    map)
  "Keymap for `magit-gh-repo-list-mode'.")

(define-derived-mode magit-gh-repo-list-mode special-mode "GH-Repos"
  "Major mode for viewing a list of GitHub repositories.

\\<magit-gh-repo-list-mode-map>\
\\[magit-gh-repo-list-view] - View info for the repository at point
\\[magit-gh-repo-list-browse] - Open the repository at point in browser
\\[magit-gh-repo-list-refresh] - Refresh the repository list
\\[quit-window] - Close the buffer"
  :group 'magit-gh
  (setq-local header-line-format
              " n/p:navigate  RET:info  v/o:browse  g:refresh  q:quit")
  (setq-local magit-gh--navigation-property 'magit-gh-repo-nwo)
  (hl-line-mode 1))

;;; Repo List Helper Functions

(defun magit-gh--repo-nwo-at-point ()
  "Get the OWNER/REPO from the text property at point."
  (get-text-property (line-beginning-position) 'magit-gh-repo-nwo))

(defun magit-gh--repo-url-at-point ()
  "Get the repository URL from the text property at point."
  (get-text-property (line-beginning-position) 'magit-gh-repo-url))

;;; Repo List Rendering

(defun magit-gh-repo-list--insert-row (repo)
  "Insert a single row for REPO alist into the current buffer."
  (let* ((nwo (or (magit-gh--unnull (alist-get 'nameWithOwner repo)) ""))
         (desc (or (magit-gh--unnull (alist-get 'description repo)) ""))
         (visibility (downcase (or (magit-gh--unnull
                                    (alist-get 'visibility repo)) "")))
         (updated (magit-gh--format-age
                   (magit-gh--unnull (alist-get 'updatedAt repo))))
         (url (or (magit-gh--unnull (alist-get 'url repo)) ""))
         (nwo-display (if (> (length nwo) 38)
                          (concat (substring nwo 0 35) "...")
                        nwo))
         (desc-display (if (> (length desc) 50)
                           (concat (substring desc 0 47) "...")
                         desc))
         (start (point)))
    (insert (propertize (format "%-40s " nwo-display)
                        'face 'magit-gh-pr-title)
            (propertize (format "%-10s " visibility)
                        'face 'magit-gh-pr-author)
            (propertize (format "%-8s " updated)
                        'face 'magit-gh-pr-age)
            (propertize desc-display 'face 'magit-gh-pr-author)
            "\n")
    (put-text-property start (point) 'magit-gh-repo-nwo nwo)
    (put-text-property start (point) 'magit-gh-repo-url url)))

(defun magit-gh-repo-list--render (buf owner repos)
  "Render repository list REPOS for OWNER into BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Repositories for %s" owner)
                            'face 'magit-gh-header)
                "\n\n")
        (if (null repos)
            (insert (propertize (format "No repositories found for %s." owner)
                                'face 'magit-gh-pr-author))
          (insert (propertize (format "%-40s %-10s %-8s %s"
                                      "Name" "Visibility" "Updated"
                                      "Description")
                              'face 'magit-gh-header)
                  "\n")
          (insert (propertize (make-string 100 ?─) 'face 'magit-gh-header)
                  "\n")
          (dolist (repo repos)
            (magit-gh-repo-list--insert-row repo)))
        (goto-char (point-min))
        (when repos (forward-line 4))))))

;;; Repo List Commands

(defun magit-gh-repo-list--display (owner args)
  "List repositories for OWNER in a dedicated buffer.
OWNER is an owner login, or empty for the authenticated user.
ARGS are `gh repo list' filter switches and options.  When ARGS
does not set a limit, `magit-gh-repo-limit' is applied."
  (magit-gh--check-gh)
  (let* ((repo-dir (magit-gh--repo-dir))
         (default-directory repo-dir)
         (owner (string-trim (or owner "")))
         (cmd (string-join
               (append
                (list "gh repo list")
                (unless (string-empty-p owner)
                  (list (shell-quote-argument owner)))
                (list "--json nameWithOwner,description,visibility,updatedAt,url")
                (mapcar #'shell-quote-argument args)
                (unless (seq-find (lambda (a) (string-prefix-p "--limit" a)) args)
                  (list (format "--limit %d" magit-gh-repo-limit))))
               " "))
         (buf (get-buffer-create "*magit-gh: Repositories*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Loading..." 'face 'magit-gh-pr-author)))
      (magit-gh-repo-list-mode)
      (setq magit-gh-repo-list--repo-dir repo-dir)
      (setq magit-gh-repo-list--owner owner)
      (setq magit-gh-repo-list--args args))
    (pop-to-buffer buf)
    (magit-gh--async-fetch
     cmd
     (lambda (data) (magit-gh-repo-list--render buf owner data))
     (lambda (msg)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (propertize msg
                                 'face 'magit-gh-pr-review-changes-requested)))))))))

(defun magit-gh-repo-list-execute (args)
  "List repositories, reading the owner and using transient ARGS.
The owner defaults to the owner of the current repository."
  (interactive (list (transient-args 'magit-gh-repo-list)))
  (magit-gh--check-gh)
  (let* ((default (magit-gh--current-repo-owner))
         (owner (read-string
                 (format-prompt "List repositories for owner" default)
                 nil nil default)))
    (magit-gh-repo-list--display owner args)))

;;;###autoload (autoload 'magit-gh-repo-list "magit-gh-repo" nil t)
(transient-define-prefix magit-gh-repo-list ()
  "List GitHub repositories for an owner."
  ["Filters"
   ("-v" "Visibility" "--visibility="
    :choices ("public" "private" "internal"))
   ("-l" "Primary language" "--language=")
   ("-t" "Topic" "--topic=")
   ("-f" "Only forks" "--fork")
   ("-s" "Only sources (non-forks)" "--source")
   ("-a" "Only archived" "--archived")
   ("-A" "Omit archived" "--no-archived")
   ("-L" "Limit" "--limit=")]
  ["Action"
   ("l" "List" magit-gh-repo-list-execute)])

(defun magit-gh-repo-list-view ()
  "View info for the repository at point in the repo list buffer."
  (interactive)
  (if-let ((nwo (magit-gh--repo-nwo-at-point)))
      (let ((default-directory magit-gh-repo-list--repo-dir))
        (magit-gh-repo-view nwo))
    (user-error "No repository at point")))

(defun magit-gh-repo-list-browse ()
  "Open the repository at point in the browser."
  (interactive)
  (if-let ((url (magit-gh--repo-url-at-point)))
      (if (string-empty-p url)
          (user-error "No URL for repository at point")
        (browse-url url))
    (user-error "No repository at point")))

(defun magit-gh-repo-list-refresh ()
  "Refresh the repository list buffer."
  (interactive)
  (let ((default-directory magit-gh-repo-list--repo-dir))
    (magit-gh-repo-list--display magit-gh-repo-list--owner
                                 magit-gh-repo-list--args)))

;;; Repo Action Commands

(defun magit-gh-repo-fork-execute (args)
  "Fork the current repository on GitHub.
ARGS are the `gh repo fork' switches and options collected by the
`magit-gh-repo-fork' transient.  By default the fork is created on
GitHub without cloning it or modifying your local git remotes.
Enabling \"Add a git remote\" sets the fork as the `origin' remote
and renames any existing `origin' to `upstream'."
  (interactive (list (transient-args 'magit-gh-repo-fork)))
  (magit-gh--check-gh)
  (let ((default-directory (magit-gh--repo-dir)))
    (message "Forking the current repository...")
    (magit-gh--run-reporting
     (string-join (cons "gh repo fork"
                        (mapcar #'shell-quote-argument args))
                  " ")
     "Forked the current repository"
     "Failed to fork the current repository")))

;;;###autoload (autoload 'magit-gh-repo-fork "magit-gh-repo" nil t)
(transient-define-prefix magit-gh-repo-fork ()
  "Fork the current repository on GitHub."
  ["Fork options"
   ("-c" "Clone the fork" "--clone")
   ("-r" "Add a git remote" "--remote")
   ("-n" "Remote name" "--remote-name=")
   ("-f" "Rename the fork" "--fork-name=")
   ("-o" "Create the fork in an organization" "--org=")
   ("-b" "Only include the default branch" "--default-branch-only")]
  ["Action"
   ("f" "Fork" magit-gh-repo-fork-execute)])

(transient-define-argument magit-gh-repo-create--visibility ()
  "Visibility switch for the `magit-gh-repo-create' transient."
  :class 'transient-switches
  :description "Visibility"
  :key "-V"
  :argument-format "--%s"
  :argument-regexp "\\(--\\(public\\|private\\|internal\\)\\)"
  :choices '("public" "private" "internal")
  :init-value (lambda (obj) (oset obj value "--private")))

(defun magit-gh-repo-create-execute (args)
  "Create a new GitHub repository.
ARGS are the `gh repo create' switches and options collected by
the `magit-gh-repo-create' transient.  The repository name is read
interactively; it may be \"owner/name\" or just \"name\" (defaulting
to the authenticated user)."
  (interactive (list (transient-args 'magit-gh-repo-create)))
  (magit-gh--check-gh)
  (let ((name (string-trim
               (read-string "New repository name (owner/name or name): "))))
    (when (string-empty-p name)
      (user-error "Repository name is required"))
    (message "Creating repository %s..." name)
    (magit-gh--run-reporting
     (string-join (append (list "gh repo create" (shell-quote-argument name))
                          (mapcar #'shell-quote-argument args))
                  " ")
     (format "Created repository %s" name)
     (format "Failed to create repository %s" name))))

;;;###autoload (autoload 'magit-gh-repo-create "magit-gh-repo" nil t)
(transient-define-prefix magit-gh-repo-create ()
  "Create a new GitHub repository."
  ["Repository options"
   (magit-gh-repo-create--visibility)
   ("-d" "Description" "--description=")
   ("-h" "Home page URL" "--homepage=")
   ("-g" "Gitignore template" "--gitignore=")
   ("-l" "License" "--license=")]
  ["After creating"
   ("-c" "Clone the new repository" "--clone")
   ("-a" "Add a README file" "--add-readme")]
  ["Action"
   ("c" "Create" magit-gh-repo-create-execute)])

;;;###autoload
(defun magit-gh-repo-sync (&optional target)
  "Sync a fork with its upstream repository.
With a prefix argument, prompt for a destination [OWNER/]REPO TARGET;
otherwise sync the current local repository from its parent."
  (interactive (list (when current-prefix-arg (magit-gh--read-repo-target))))
  (magit-gh--check-gh)
  (let* ((default-directory (magit-gh--repo-dir))
         (label (or target "the current repository")))
    (message "Syncing %s..." label)
    (magit-gh--run-reporting
     (concat "gh repo sync" (magit-gh--repo-target-arg target))
     (format "Synced %s" label)
     (format "Failed to sync %s" label))))

;;;###autoload
(defun magit-gh-repo-set-default (repo)
  "Set the default repository for `gh' commands in the current directory.
REPO is an OWNER/REPO string, chosen interactively from the
current repository, its fork parent, and the repository's GitHub
remotes (or entered manually).  With a prefix argument, show the
current default repository instead of setting it."
  (interactive
   (list (if current-prefix-arg
             'view
           (let ((candidates (magit-gh--default-repo-candidates)))
             (completing-read "Set default repo: " candidates nil nil
                              nil nil (car candidates))))))
  (magit-gh--check-gh)
  (let ((default-directory (magit-gh--repo-dir)))
    (if (eq repo 'view)
        (let ((out (string-trim
                    (shell-command-to-string "gh repo set-default --view 2>&1"))))
          (message "%s" (if (string-empty-p out)
                            "No default repository set."
                          (format "Default repository: %s" out))))
      (when (or (null repo) (string-empty-p (string-trim repo)))
        (user-error "No repository specified"))
      (magit-gh--run-reporting
       (concat "gh repo set-default " (shell-quote-argument (string-trim repo)))
       (format "Default repository set to %s" repo)
       (format "Failed to set default repository to %s" repo)))))

(provide 'magit-gh-repo)

;;; magit-gh-repo.el ends here
