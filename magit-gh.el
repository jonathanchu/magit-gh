;;; magit-gh.el --- GitHub CLI integration for Magit -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/magit-gh
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1") (magit "4.0.0") (transient "0.5.0"))
;; Keywords: git tools vc github

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

;; A lightweight GitHub CLI (gh) integration for Magit.
;; Provides commands to list and checkout pull requests
;; using the `gh` CLI tool (https://cli.github.com).
;;
;; Usage:
;;   Press ",'" in Magit buffers to open the GitHub CLI menu.
;;
;; Setup with use-package:
;;
;;   (use-package magit-gh
;;     :ensure t
;;     :after magit)
;;
;; To use a custom keybinding, set the variable before
;; the package loads:
;;
;;   (use-package magit-gh
;;     :ensure t
;;     :after magit
;;     :init
;;     (setq magit-gh-key ";"))
;;
;; To increase the number of PRs fetched (default 30):
;;
;;   (use-package magit-gh
;;     :ensure t
;;     :after magit
;;     :custom
;;     (magit-gh-pr-limit 50))
;;
;; Requires: gh must be installed and authenticated.
;; Run `gh auth login` to authenticate.
;; See: https://cli.github.com

;;; Code:

(require 'cl-lib)
(require 'magit)
(require 'transient)
(require 'json)
(require 'diff-mode)
(require 'iso8601)

;;; Custom Variables

(defgroup magit-gh nil
  "GitHub CLI integration for Magit."
  :prefix "magit-gh-"
  :group 'magit-extensions)

(defcustom magit-gh-key ","
  "Key to bind `magit-gh' in Magit buffers.
Set this variable before loading the package to use a custom key."
  :type 'string
  :group 'magit-gh)

(defcustom magit-gh-pr-limit 30
  "Maximum number of PRs to fetch when listing."
  :type 'integer
  :group 'magit-gh)

;;; Transient Menu

;;;###autoload (autoload 'magit-gh "magit-gh" nil t)
(transient-define-prefix magit-gh ()
  "GitHub CLI commands."
  ["Overview"
   ("l" "List PRs" magit-gh-pr-list)
   ("s" "PR status dashboard" magit-gh-pr-status)]
  ["Inspect"
   ("d" "Diff PR" magit-gh-pr-diff)
   ("k" "PR checks/CI status" magit-gh-pr-checks)]
  ["Actions"
   ("c" "Checkout PR" magit-gh-pr-checkout)
   ("v" "View PR in browser" magit-gh-pr-view)])

;;; PR List Buffer Mode

(defvar-local magit-gh-pr-list--repo-dir nil
  "The repository directory for the current PR list buffer.")

(defvar-local magit-gh-pr-list--state "open"
  "The current state filter for the PR list buffer.
One of \"open\", \"closed\", \"merged\", or \"all\".")

(defvar magit-gh-pr-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'magit-gh-pr-list-checkout)
    (define-key map (kbd "c") #'magit-gh-pr-list-checkout)
    (define-key map (kbd "d") #'magit-gh-pr-list-diff)
    (define-key map (kbd "v") #'magit-gh-pr-list-browse)
    (define-key map (kbd "k") #'magit-gh-pr-list-checks)
    (define-key map (kbd "n") #'magit-gh--next-item)
    (define-key map (kbd "p") #'magit-gh--previous-item)
    (define-key map (kbd "t") #'magit-gh-pr-list-cycle-state)
    (define-key map (kbd "g") #'magit-gh-pr-list-refresh)
    map)
  "Keymap for `magit-gh-pr-list-mode'.")

(define-derived-mode magit-gh-pr-list-mode special-mode "GH-PRs"
  "Major mode for viewing GitHub pull requests.

\\<magit-gh-pr-list-mode-map>\
\\[magit-gh-pr-list-checkout] - Checkout the PR at point
\\[magit-gh-pr-list-browse] - Open the PR at point in browser
\\[magit-gh-pr-list-refresh] - Refresh the PR list
\\[quit-window] - Close the buffer"
  :group 'magit-gh
  (magit-gh-pr-list--update-header-line)
  (hl-line-mode 1))

;;; Custom faces

(defface magit-gh-pr-number
  '((t :inherit magit-hash))
  "Face for PR numbers in the PR list."
  :group 'magit-gh)

(defface magit-gh-pr-title
  '((t :inherit default))
  "Face for PR titles in the PR list."
  :group 'magit-gh)

(defface magit-gh-pr-author
  '((t :inherit magit-dimmed))
  "Face for PR authors in the PR list."
  :group 'magit-gh)

(defface magit-gh-pr-branch
  '((t :inherit magit-branch-remote))
  "Face for PR branch names in the PR list."
  :group 'magit-gh)

(defface magit-gh-pr-review-approved
  '((t :inherit success))
  "Face for approved review status in the PR list."
  :group 'magit-gh)

(defface magit-gh-pr-review-changes-requested
  '((t :inherit error))
  "Face for changes-requested review status in the PR list."
  :group 'magit-gh)

(defface magit-gh-pr-review-pending
  '((t :inherit warning))
  "Face for review-required status in the PR list."
  :group 'magit-gh)

(defface magit-gh-header
  '((t :inherit magit-section-heading))
  "Face for the header line in the PR list."
  :group 'magit-gh)

(defface magit-gh-pr-age
  '((t :inherit magit-dimmed))
  "Face for age and merged columns in the PR list."
  :group 'magit-gh)

;;; Navigation

(defun magit-gh--next-item ()
  "Move point to the next item row."
  (interactive)
  (let ((start (point))
        (prop (if (derived-mode-p 'magit-gh-pr-checks-mode)
                  'magit-gh-check-link
                'magit-gh-pr-number)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (line-beginning-position) prop)))
      (forward-line 1))
    (unless (get-text-property (line-beginning-position) prop)
      (goto-char start))))

(defun magit-gh--previous-item ()
  "Move point to the previous item row."
  (interactive)
  (let ((start (point))
        (prop (if (derived-mode-p 'magit-gh-pr-checks-mode)
                  'magit-gh-check-link
                'magit-gh-pr-number)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (line-beginning-position) prop)))
      (forward-line -1))
    (unless (get-text-property (line-beginning-position) prop)
      (goto-char start))))

;;; Helper functions

(defun magit-gh--repo-dir ()
  "Return the toplevel directory of the current repository."
  (or (magit-toplevel)
      (user-error "Not inside a Git repository")))

(defun magit-gh--check-gh ()
  "Ensure the gh CLI is available."
  (unless (executable-find "gh")
    (user-error "`gh' not found; install from https://cli.github.com")))

(defun magit-gh--fetch-prs (&optional state)
  "Fetch PRs as a list of alists via gh CLI.
STATE is one of \"open\", \"closed\", \"merged\", or \"all\" (default \"open\")."
  (magit-gh--check-gh)
  (let* ((default-directory (magit-gh--repo-dir))
         (state (or state "open"))
         (cmd (format "gh pr list --state %s --json number,title,author,headRefName,reviewDecision,createdAt --limit %d"
                      state magit-gh-pr-limit))
         (output (shell-command-to-string cmd))
         (trimmed (string-trim output)))
    (if (string-prefix-p "[" trimmed)
        (json-parse-string trimmed :array-type 'list :object-type 'alist)
      (user-error "Failed to fetch PRs: %s" trimmed))))

(defun magit-gh--fetch-pr-status ()
  "Fetch PR status via gh CLI.
Returns an alist with `createdBy' and `needsReview' keys,
each containing a list of PR alists."
  (magit-gh--check-gh)
  (let* ((default-directory (magit-gh--repo-dir))
         (cmd "gh pr status --json number,title,author,headRefName,reviewDecision,createdAt")
         (output (shell-command-to-string cmd))
         (trimmed (string-trim output)))
    (if (string-prefix-p "{" trimmed)
        (json-parse-string trimmed :array-type 'list :object-type 'alist)
      (user-error "Failed to fetch PR status: %s" trimmed))))

(defun magit-gh--fetch-recently-merged ()
  "Fetch recently merged PRs for the current repository via gh CLI.
Returns a list of PR alists."
  (magit-gh--check-gh)
  (let* ((default-directory (magit-gh--repo-dir))
         (cmd "gh pr list --state merged --json number,title,author,headRefName,reviewDecision,createdAt,mergedAt --limit 10")
         (output (shell-command-to-string cmd))
         (trimmed (string-trim output)))
    (if (string-prefix-p "[" trimmed)
        (json-parse-string trimmed :array-type 'list :object-type 'alist)
      (user-error "Failed to fetch recently merged PRs: %s" trimmed))))

(defun magit-gh--async-fetch (cmd callback &optional errback)
  "Run CMD asynchronously, parse JSON output, and call CALLBACK.
CMD is a shell command string (typically a gh CLI invocation).
CALLBACK is called with the parsed JSON data on success.
ERRBACK is called with an error message on failure; if nil,
a message is displayed instead."
  (let* ((output (list ""))
         (stderr-buf (generate-new-buffer " *magit-gh-async-stderr*"))
         (coding-system-for-read 'utf-8-unix)
         (process-environment (cons "NO_COLOR=1" process-environment))
         (proc (make-process
                :name "magit-gh-async"
                :buffer nil
                :stderr stderr-buf
                :command (split-string cmd)
                :filter
                (lambda (_process string)
                  (setcar output (concat (car output) string)))
                :sentinel
                (lambda (process _event)
                  (when (memq (process-status process) '(exit signal))
                    (unwind-protect
                        (if (= (process-exit-status process) 0)
                            (condition-case err
                                (let* ((trimmed (string-trim (car output)))
                                       (data (json-parse-string
                                              trimmed
                                              :array-type 'list
                                              :object-type 'alist)))
                                  (funcall callback data))
                              (json-parse-error
                               (let ((msg (format "magit-gh: %s"
                                                  (error-message-string err))))
                                 (if errback (funcall errback msg)
                                   (message "%s" msg)))))
                          (let ((msg (format "gh command failed: %s"
                                             (string-trim
                                              (with-current-buffer stderr-buf
                                                (buffer-string))))))
                            (if errback
                                (funcall errback msg)
                              (message "%s" msg))))
                      (kill-buffer stderr-buf)))))))
    (ignore proc)))

(defun magit-gh--pr-number-at-point ()
  "Get the PR number from the text property at point."
  (get-text-property (line-beginning-position) 'magit-gh-pr-number))

(defun magit-gh--checkout-pr (number)
  "Checkout PR by NUMBER using gh CLI.
On success, closes the PR list buffer and opens `magit-status'
so the branch change is immediately visible."
  (let ((default-directory (magit-gh--repo-dir)))
    (message "Checking out PR #%d..." number)
    (let ((exit-code (call-process-shell-command
                      (format "gh pr checkout %d" number)
                      nil "*magit-gh-output*" nil)))
      (if (= exit-code 0)
          (progn
            (when-let ((buf (get-buffer "*magit-gh: Pull Requests*")))
              (quit-window nil (get-buffer-window buf)))
            (when-let ((buf (get-buffer "*magit-gh: PR Status*")))
              (quit-window nil (get-buffer-window buf)))
            (magit-status-setup-buffer default-directory)
            (message "Checked out PR #%d" number))
        (user-error "Failed to checkout PR #%d; see *magit-gh-output* buffer"
                    number)))))

(defun magit-gh--review-decision-display (decision)
  "Return a propertized string for review DECISION."
  (pcase decision
    ("APPROVED"
     (propertize "Approved" 'face 'magit-gh-pr-review-approved))
    ("CHANGES_REQUESTED"
     (propertize "Changes requested" 'face 'magit-gh-pr-review-changes-requested))
    ("REVIEW_REQUIRED"
     (propertize "Review required" 'face 'magit-gh-pr-review-pending))
    (_
     (propertize "—" 'face 'magit-gh-pr-author))))

(defun magit-gh--format-age (iso-timestamp)
  "Format ISO-TIMESTAMP as a compact age string.
Returns \"<1d\", \"3d\", \"2w\", \"3mo\", or \"1y\".
Returns \"\" for nil input."
  (if (null iso-timestamp)
      ""
    (let* ((parsed (iso8601-parse iso-timestamp))
           (time (encode-time parsed))
           (days (/ (float-time (time-subtract nil time)) 86400)))
      (cond
       ((< days 1) "<1d")
       ((< days 14) (format "%dd" (floor days)))
       ((< days 60) (format "%dw" (floor (/ days 7))))
       ((< days 365) (format "%dmo" (floor (/ days 30))))
       (t (format "%dy" (floor (/ days 365))))))))

(defun magit-gh--pr-choices (prs)
  "Build an alist of display strings to PR numbers from PRS."
  (mapcar (lambda (pr)
            (let ((number (alist-get 'number pr))
                  (title (alist-get 'title pr)))
              (cons (format "#%-5d %s" number title)
                    number)))
          prs))

(defun magit-gh--insert-pr-header (&optional show-merged-col)
  "Insert column header and separator line for a PR table.
When SHOW-MERGED-COL is non-nil, append a \"Merged\" column."
  (insert (propertize (format "%-7s %-5s %-50s %-20s %-20s %s"
                              "PR" "Age" "Title" "Author" "Review" "Branch")
                      'face 'magit-gh-header))
  (when show-merged-col
    (insert (propertize (format "  %-6s" "Merged") 'face 'magit-gh-header)))
  (insert "\n")
  (insert (propertize (make-string (if show-merged-col 144 136) ?─)
                      'face 'magit-gh-header)
          "\n"))

(defun magit-gh--insert-pr-row (pr &optional show-merged-col)
  "Insert a single PR row with text properties for PR alist.
When SHOW-MERGED-COL is non-nil, append the merged age at end of row."
  (let* ((number (alist-get 'number pr))
         (title (alist-get 'title pr))
         (author (alist-get 'login (alist-get 'author pr)))
         (review (alist-get 'reviewDecision pr))
         (branch (alist-get 'headRefName pr))
         (created-at (alist-get 'createdAt pr))
         (age-display (magit-gh--format-age created-at))
         (title-display (if (> (length title) 48)
                            (concat (substring title 0 45) "...")
                          title))
         (review-display (magit-gh--review-decision-display review))
         (start (point)))
    (insert (propertize (format "%-7s " (format "#%d" number))
                        'face 'magit-gh-pr-number)
            (propertize (format "%-5s " age-display)
                        'face 'magit-gh-pr-age)
            (propertize (format "%-50s " title-display)
                        'face 'magit-gh-pr-title)
            (propertize (format "%-20s " (or author ""))
                        'face 'magit-gh-pr-author)
            (format "%-20s " review-display)
            (propertize (or branch "")
                        'face 'magit-gh-pr-branch))
    (when show-merged-col
      (let ((merged-at (alist-get 'mergedAt pr)))
        (insert (propertize (format "  %-6s" (magit-gh--format-age merged-at))
                            'face 'magit-gh-pr-age))))
    (insert "\n")
    (put-text-property start (point) 'magit-gh-pr-number number)))

;;; PR Diff Mode

(defvar-local magit-gh-pr-diff--repo-dir nil
  "The repository directory for the current PR diff buffer.")

(defvar magit-gh-pr-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `magit-gh-pr-diff-mode'.")

(define-derived-mode magit-gh-pr-diff-mode diff-mode "GH-Diff"
  "Major mode for viewing a GitHub PR diff."
  :group 'magit-gh)

(defun magit-gh--show-pr-diff (number)
  "Display the diff for PR NUMBER in a dedicated buffer."
  (let* ((default-directory (magit-gh--repo-dir))
         (repo-dir default-directory)
         (output (shell-command-to-string (format "gh pr diff %d" number)))
         (buf (get-buffer-create (format "*magit-gh: PR #%d Diff*" number))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output))
      (magit-gh-pr-diff-mode)
      (setq magit-gh-pr-diff--repo-dir repo-dir)
      (setq-local header-line-format (format " PR #%d Diff" number))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun magit-gh-pr-list--update-header-line ()
  "Update the header line to reflect the current state filter."
  (setq-local header-line-format
              " n/p:navigate  c:checkout  d:diff  v:browse  k:checks  t:toggle state  g:refresh  q:quit"))

(defun magit-gh-pr-list--state-indicator (active-state)
  "Return a propertized state indicator string for ACTIVE-STATE.
Highlights the active state and dims the others."
  (let ((states '("open" "closed" "merged" "all")))
    (concat "  ["
            (mapconcat
             (lambda (s)
               (if (equal s active-state)
                   (propertize s 'face 'magit-gh-header)
                 (propertize s 'face 'magit-gh-pr-author)))
             states
             " · ")
            "]")))

(defun magit-gh-pr-list-cycle-state ()
  "Cycle the PR list state filter: open -> closed -> merged -> all -> open."
  (interactive)
  (setq magit-gh-pr-list--state
        (pcase magit-gh-pr-list--state
          ("open" "closed")
          ("closed" "merged")
          ("merged" "all")
          (_ "open")))
  (magit-gh-pr-list-refresh))

;;; PR List Buffer Commands

(defun magit-gh-pr-list-checkout ()
  "Checkout the PR at point."
  (interactive)
  (if-let ((number (magit-gh--pr-number-at-point)))
      (let ((default-directory magit-gh-pr-list--repo-dir))
        (magit-gh--checkout-pr number))
    (user-error "No PR at point")))

(defun magit-gh-pr-list-diff ()
  "View the diff for the PR at point."
  (interactive)
  (if-let ((number (magit-gh--pr-number-at-point)))
      (let ((default-directory magit-gh-pr-list--repo-dir))
        (magit-gh--show-pr-diff number))
    (user-error "No PR at point")))

(defun magit-gh-pr-list-browse ()
  "Open the PR at point in the browser."
  (interactive)
  (if-let ((number (magit-gh--pr-number-at-point)))
      (let ((default-directory magit-gh-pr-list--repo-dir))
        (shell-command (format "gh pr view %d --web" number)))
    (user-error "No PR at point")))

(defun magit-gh-pr-list-refresh ()
  "Refresh the PR list buffer, preserving the current state filter."
  (interactive)
  (let ((default-directory magit-gh-pr-list--repo-dir)
        (state magit-gh-pr-list--state))
    (magit-gh-pr-list state)))

;;; PR Status Buffer Mode

(defvar-local magit-gh-pr-status--repo-dir nil
  "The repository directory for the current PR status buffer.")

(defvar magit-gh-pr-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'magit-gh-pr-status-checkout)
    (define-key map (kbd "c") #'magit-gh-pr-status-checkout)
    (define-key map (kbd "d") #'magit-gh-pr-status-diff)
    (define-key map (kbd "v") #'magit-gh-pr-status-browse)
    (define-key map (kbd "k") #'magit-gh-pr-status-checks)
    (define-key map (kbd "n") #'magit-gh--next-item)
    (define-key map (kbd "p") #'magit-gh--previous-item)
    (define-key map (kbd "g") #'magit-gh-pr-status-refresh)
    map)
  "Keymap for `magit-gh-pr-status-mode'.")

(define-derived-mode magit-gh-pr-status-mode special-mode "GH-Status"
  "Major mode for viewing GitHub PR status dashboard.

\\<magit-gh-pr-status-mode-map>\
\\[magit-gh-pr-status-checkout] - Checkout the PR at point
\\[magit-gh-pr-status-browse] - Open the PR at point in browser
\\[magit-gh-pr-status-refresh] - Refresh the status dashboard
\\[quit-window] - Close the buffer"
  :group 'magit-gh
  (setq-local header-line-format
              (substitute-command-keys
               "\\<magit-gh-pr-status-mode-map>\
 \\[magit-gh--next-item]/\\[magit-gh--previous-item]:navigate  \
\\[magit-gh-pr-status-checkout]:checkout  \
\\[magit-gh-pr-status-diff]:diff  \
\\[magit-gh-pr-status-browse]:browse  \
\\[magit-gh-pr-status-checks]:checks  \
\\[magit-gh-pr-status-refresh]:refresh  \
\\[quit-window]:quit"))
  (hl-line-mode 1))

;;; PR Status Buffer Commands

(defun magit-gh-pr-status-checkout ()
  "Checkout the PR at point in the status buffer."
  (interactive)
  (if-let ((number (magit-gh--pr-number-at-point)))
      (let ((default-directory magit-gh-pr-status--repo-dir))
        (magit-gh--checkout-pr number))
    (user-error "No PR at point")))

(defun magit-gh-pr-status-diff ()
  "View the diff for the PR at point in the status buffer."
  (interactive)
  (if-let ((number (magit-gh--pr-number-at-point)))
      (let ((default-directory magit-gh-pr-status--repo-dir))
        (magit-gh--show-pr-diff number))
    (user-error "No PR at point")))

(defun magit-gh-pr-status-browse ()
  "Open the PR at point in the browser from the status buffer."
  (interactive)
  (if-let ((number (magit-gh--pr-number-at-point)))
      (let ((default-directory magit-gh-pr-status--repo-dir))
        (shell-command (format "gh pr view %d --web" number)))
    (user-error "No PR at point")))

(defun magit-gh-pr-status-refresh ()
  "Refresh the PR status buffer."
  (interactive)
  (let ((default-directory magit-gh-pr-status--repo-dir))
    (magit-gh-pr-status)))

;;; Main Commands

(defun magit-gh-pr-list--render (buf state prs)
  "Render PR list data PRS into BUF for STATE."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "%s pull requests"
                                    (capitalize state))
                            'face 'magit-gh-header)
                (magit-gh-pr-list--state-indicator state)
                "\n\n")
        (if (null prs)
            (insert (propertize (format "No %s pull requests." state)
                                'face 'magit-gh-pr-author))
          (magit-gh--insert-pr-header)
          (dolist (pr prs)
            (magit-gh--insert-pr-row pr)))
        (goto-char (point-min))
        (when prs (forward-line 4))))))

(defun magit-gh-pr-list (&optional state)
  "List pull requests for the current repository.
STATE is one of \"open\", \"closed\", \"merged\", or \"all\" (default \"open\")."
  (interactive)
  (magit-gh--check-gh)
  (let* ((state (or state "open"))
         (repo-dir (magit-gh--repo-dir))
         (default-directory repo-dir)
         (cmd (format "gh pr list --state %s --json number,title,author,headRefName,reviewDecision,createdAt --limit %d"
                      state magit-gh-pr-limit))
         (buf (get-buffer-create "*magit-gh: Pull Requests*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "%s pull requests"
                                    (capitalize state))
                            'face 'magit-gh-header)
                (magit-gh-pr-list--state-indicator state)
                "\n\n")
        (insert (propertize "Loading..." 'face 'magit-gh-pr-author)))
      (magit-gh-pr-list-mode)
      (setq magit-gh-pr-list--repo-dir repo-dir)
      (setq magit-gh-pr-list--state state)
      (magit-gh-pr-list--update-header-line))
    (pop-to-buffer buf)
    (magit-gh--async-fetch cmd
                           (lambda (data)
                             (magit-gh-pr-list--render buf state data)))))

(defun magit-gh-pr-status--render (buf status-data merged-data)
  "Render the PR status dashboard into BUF.
STATUS-DATA is the parsed `gh pr status' output.
MERGED-DATA is the parsed recently-merged PR list."
  (when (buffer-live-p buf)
    (let ((created (alist-get 'createdBy status-data))
          (needs-review (alist-get 'needsReview status-data)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; Created by you
          (insert (propertize "Created by you\n" 'face 'magit-gh-header))
          (if (null created)
              (insert (propertize "None.\n" 'face 'magit-gh-pr-author))
            (magit-gh--insert-pr-header)
            (dolist (pr created)
              (magit-gh--insert-pr-row pr)))
          (insert "\n")
          ;; Review requested
          (insert (propertize "Review requested\n" 'face 'magit-gh-header))
          (if (null needs-review)
              (insert (propertize "None.\n" 'face 'magit-gh-pr-author))
            (magit-gh--insert-pr-header)
            (dolist (pr needs-review)
              (magit-gh--insert-pr-row pr)))
          (insert "\n")
          ;; Recently merged
          (insert (propertize "Recently merged\n" 'face 'magit-gh-header))
          (if (null merged-data)
              (insert (propertize "None.\n" 'face 'magit-gh-pr-author))
            (magit-gh--insert-pr-header t)
            (dolist (pr merged-data)
              (magit-gh--insert-pr-row pr t))))
        (goto-char (point-min))))))

(defun magit-gh-pr-status ()
  "Show PR status dashboard for the current repository.
Displays PRs created by you, PRs awaiting your review,
and your recently merged PRs."
  (interactive)
  (magit-gh--check-gh)
  (let* ((repo-dir (magit-gh--repo-dir))
         (default-directory repo-dir)
         (status-cmd "gh pr status --json number,title,author,headRefName,reviewDecision,createdAt")
         (merged-cmd "gh pr list --state merged --json number,title,author,headRefName,reviewDecision,createdAt,mergedAt --limit 10")
         (buf (get-buffer-create "*magit-gh: PR Status*"))
         (results (list nil nil))
         (remaining (list 2)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Loading..." 'face 'magit-gh-pr-author)))
      (magit-gh-pr-status-mode)
      (setq magit-gh-pr-status--repo-dir repo-dir))
    (pop-to-buffer buf)
    (magit-gh--async-fetch
     status-cmd
     (lambda (data)
       (setcar results data)
       (cl-decf (car remaining))
       (when (= (car remaining) 0)
         (magit-gh-pr-status--render buf (nth 0 results) (nth 1 results)))))
    (magit-gh--async-fetch
     merged-cmd
     (lambda (data)
       (setcar (cdr results) data)
       (cl-decf (car remaining))
       (when (= (car remaining) 0)
         (magit-gh-pr-status--render buf (nth 0 results) (nth 1 results)))))))

(defun magit-gh-pr-checkout ()
  "Select and checkout a PR using `completing-read'."
  (interactive)
  (let* ((prs (magit-gh--fetch-prs))
         (choices (magit-gh--pr-choices prs))
         (choice (completing-read "Checkout PR: " choices nil t))
         (number (cdr (assoc choice choices))))
    (when number
      (magit-gh--checkout-pr number))))

(defun magit-gh-pr-diff ()
  "Select a PR and view its diff."
  (interactive)
  (let* ((prs (magit-gh--fetch-prs))
         (choices (magit-gh--pr-choices prs))
         (choice (completing-read "Diff PR: " choices nil t))
         (number (cdr (assoc choice choices))))
    (when number
      (magit-gh--show-pr-diff number))))

(defun magit-gh-pr-view ()
  "Select a PR and open it in the browser."
  (interactive)
  (let* ((prs (magit-gh--fetch-prs))
         (choices (magit-gh--pr-choices prs))
         (choice (completing-read "View PR: " choices nil t))
         (number (cdr (assoc choice choices))))
    (when number
      (let ((default-directory (magit-gh--repo-dir)))
        (shell-command (format "gh pr view %d --web" number))))))

;;; PR Checks Buffer Mode

(defvar-local magit-gh-pr-checks--repo-dir nil
  "The repository directory for the current PR checks buffer.")

(defvar-local magit-gh-pr-checks--pr-number nil
  "The PR number for the current PR checks buffer.")

(defvar magit-gh-pr-checks-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "v") #'magit-gh-pr-checks-browse)
    (define-key map (kbd "n") #'magit-gh--next-item)
    (define-key map (kbd "p") #'magit-gh--previous-item)
    (define-key map (kbd "g") #'magit-gh-pr-checks-refresh)
    map)
  "Keymap for `magit-gh-pr-checks-mode'.")

(define-derived-mode magit-gh-pr-checks-mode special-mode "GH-Checks"
  "Major mode for viewing GitHub PR checks/CI status.

\\<magit-gh-pr-checks-mode-map>\
\\[magit-gh-pr-checks-browse] - Open the check at point in browser
\\[magit-gh-pr-checks-refresh] - Refresh the checks
\\[quit-window] - Close the buffer"
  :group 'magit-gh
  (setq-local header-line-format " n/p:navigate  v:browse  g:refresh  q:quit")
  (hl-line-mode 1))

;;; PR Checks Helper Functions

(defun magit-gh--format-duration (started completed)
  "Format elapsed time between STARTED and COMPLETED ISO timestamps.
Returns \"Xs\", \"Xm Ys\", or \"Xh Ym\".
Returns \"--\" if either timestamp is nil."
  (if (or (not (stringp started)) (not (stringp completed))
          (string-empty-p started) (string-empty-p completed)
          (string-prefix-p "0001" started)
          (string-prefix-p "0001" completed))
      "--"
    (let* ((start-time (encode-time (iso8601-parse started)))
           (end-time (encode-time (iso8601-parse completed)))
           (secs (floor (float-time (time-subtract end-time start-time)))))
      (if (<= secs 0)
          "--"
        (cond
         ((< secs 60) (format "%ds" secs))
         ((< secs 3600) (format "%dm %ds" (/ secs 60) (% secs 60)))
         (t (format "%dh %dm" (/ secs 3600) (/ (% secs 3600) 60))))))))

(defun magit-gh--check-bucket-display (bucket)
  "Return a propertized string for check BUCKET status."
  (pcase bucket
    ("pass"
     (propertize "pass" 'face 'magit-gh-pr-review-approved))
    ((or "fail" "cancel")
     (propertize bucket 'face 'magit-gh-pr-review-changes-requested))
    ("pending"
     (propertize "pending" 'face 'magit-gh-pr-review-pending))
    ("skipping"
     (propertize "skipping" 'face 'magit-gh-pr-author))
    (_
     (propertize (or bucket "—") 'face 'magit-gh-pr-author))))

(defun magit-gh--check-link-at-point ()
  "Get the check link from the text property at point."
  (get-text-property (line-beginning-position) 'magit-gh-check-link))

;;; PR Checks Rendering

(defun magit-gh--check-sort-key (bucket)
  "Return a numeric sort key for BUCKET (lower = first)."
  (pcase bucket
    ("fail" 0)
    ("cancel" 1)
    ("pending" 2)
    ("pass" 3)
    ("skipping" 4)
    (_ 5)))

(defun magit-gh-pr-checks--insert-row (check)
  "Insert a single row for CHECK alist into the current buffer."
  (let* ((bucket (alist-get 'bucket check))
         (name (or (alist-get 'name check) ""))
         (workflow (or (alist-get 'workflow check) ""))
         (started (alist-get 'startedAt check))
         (completed (alist-get 'completedAt check))
         (duration (magit-gh--format-duration started completed))
         (description (or (alist-get 'description check) ""))
         (link (or (alist-get 'link check) ""))
         (name-display (if (> (length name) 38)
                           (concat (substring name 0 35) "...")
                         name))
         (workflow-display (if (> (length workflow) 23)
                               (concat (substring workflow 0 20) "...")
                             workflow))
         (desc-display (if (> (length description) 40)
                           (concat (substring description 0 37) "...")
                         description))
         (start (point)))
    (insert (format "%-10s " (magit-gh--check-bucket-display bucket))
            (propertize (format "%-40s " name-display)
                        'face 'magit-gh-pr-title)
            (propertize (format "%-25s " workflow-display)
                        'face 'magit-gh-pr-author)
            (propertize (format "%-10s " duration)
                        'face 'magit-gh-pr-age)
            (propertize desc-display
                        'face 'magit-gh-pr-author)
            "\n")
    (put-text-property start (point) 'magit-gh-check-link link)))

(defun magit-gh-pr-checks--render (buf pr-number checks)
  "Render PR checks data CHECKS into BUF for PR-NUMBER.
PR-NUMBER may be nil for current-branch checks."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (sorted (sort (copy-sequence checks)
                          (lambda (a b)
                            (< (magit-gh--check-sort-key
                                (alist-get 'bucket a))
                               (magit-gh--check-sort-key
                                (alist-get 'bucket b)))))))
        (erase-buffer)
        (insert (propertize (if pr-number
                                (format "Checks for PR #%d" pr-number)
                              "Checks for current branch")
                            'face 'magit-gh-header)
                "\n\n")
        (if (null sorted)
            (insert (propertize "No checks found." 'face 'magit-gh-pr-author))
          (insert (propertize (format "%-10s %-40s %-25s %-10s %s"
                                      "Status" "Name" "Workflow"
                                      "Duration" "Description")
                              'face 'magit-gh-header)
                  "\n")
          (insert (propertize (make-string 100 ?─) 'face 'magit-gh-header)
                  "\n")
          (dolist (check sorted)
            (magit-gh-pr-checks--insert-row check)))
        (goto-char (point-min))
        (when sorted (forward-line 4))))))

;;; PR Checks Commands

(defun magit-gh--show-pr-checks (number)
  "Show CI checks for PR NUMBER.
If NUMBER is nil, show checks for the current branch's PR."
  (magit-gh--check-gh)
  (let* ((repo-dir (magit-gh--repo-dir))
         (default-directory repo-dir)
         (repo-name (file-name-nondirectory (directory-file-name repo-dir)))
         (buf-name (if number
                       (format "*magit-gh: %s PR #%d Checks*" repo-name number)
                     (format "*magit-gh: %s PR Checks*" repo-name)))
         (cmd (format "gh pr checks %s --json bucket,name,description,completedAt,startedAt,link,workflow"
                      (if number (number-to-string number) "")))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Loading..." 'face 'magit-gh-pr-author)))
      (magit-gh-pr-checks-mode)
      (setq magit-gh-pr-checks--repo-dir repo-dir)
      (setq magit-gh-pr-checks--pr-number number))
    (pop-to-buffer buf)
    (magit-gh--async-fetch
     cmd
     (lambda (data)
       (magit-gh-pr-checks--render buf number data))
     (lambda (msg)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let* ((inhibit-read-only t)
                  (friendly (cond
                             ((string-match-p "no pull request" msg)
                              "No pull request found for the current branch.")
                             ((string-match-p "Could not resolve" msg)
                              "Could not find this repository on GitHub.")
                             ((string-match-p "gh auth" msg)
                              "Not authenticated. Run `gh auth login` first.")
                             (t msg))))
             (erase-buffer)
             (insert (propertize friendly 'face 'magit-gh-pr-review-changes-requested)))))))))

(defun magit-gh-pr-checks ()
  "Show CI checks for the current branch's PR."
  (interactive)
  (magit-gh--show-pr-checks nil))

(defun magit-gh-pr-list-checks ()
  "Show CI checks for the PR at point in the PR list buffer."
  (interactive)
  (if-let ((number (magit-gh--pr-number-at-point)))
      (let ((default-directory magit-gh-pr-list--repo-dir))
        (magit-gh--show-pr-checks number))
    (user-error "No PR at point")))

(defun magit-gh-pr-status-checks ()
  "Show CI checks for the PR at point in the PR status buffer."
  (interactive)
  (if-let ((number (magit-gh--pr-number-at-point)))
      (let ((default-directory magit-gh-pr-status--repo-dir))
        (magit-gh--show-pr-checks number))
    (user-error "No PR at point")))

;;; PR Checks Buffer Commands

(defun magit-gh-pr-checks-browse ()
  "Open the check at point in the browser."
  (interactive)
  (if-let ((link (magit-gh--check-link-at-point)))
      (if (string-empty-p link)
          (user-error "No link for check at point")
        (browse-url link))
    (user-error "No check at point")))

(defun magit-gh-pr-checks-refresh ()
  "Refresh the PR checks buffer."
  (interactive)
  (let ((default-directory magit-gh-pr-checks--repo-dir))
    (magit-gh--show-pr-checks magit-gh-pr-checks--pr-number)))

;;; Integration with Magit

(transient-append-suffix 'magit-dispatch "!"
  `(,magit-gh-key "GitHub" magit-gh))

(define-key magit-mode-map (kbd magit-gh-key) #'magit-gh)

(provide 'magit-gh)

;;; magit-gh.el ends here
