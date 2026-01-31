;;; magit-gh.el --- GitHub CLI integration for Magit -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/magit-gh
;; Version: 0.1.0
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

(require 'magit)
(require 'transient)
(require 'json)

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
  ["Pull Requests"
   ("l" "List open PRs" magit-gh-pr-list)
   ("c" "Checkout PR" magit-gh-pr-checkout)
   ("v" "View PR in browser" magit-gh-pr-view)])

;;; PR List Buffer Mode

(defvar-local magit-gh-pr-list--repo-dir nil
  "The repository directory for the current PR list buffer.")

(defvar magit-gh-pr-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'magit-gh-pr-list-checkout)
    (define-key map (kbd "c") #'magit-gh-pr-list-checkout)
    (define-key map (kbd "v") #'magit-gh-pr-list-browse)
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
  (setq-local header-line-format
              (substitute-command-keys
               "\\<magit-gh-pr-list-mode-map>\
 \\[magit-gh-pr-list-checkout]:checkout  \
\\[magit-gh-pr-list-browse]:browse  \
\\[magit-gh-pr-list-refresh]:refresh  \
\\[quit-window]:quit"))
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

(defface magit-gh-header
  '((t :inherit magit-section-heading))
  "Face for the header line in the PR list."
  :group 'magit-gh)

;;; Helper functions

(defun magit-gh--repo-dir ()
  "Return the toplevel directory of the current repository."
  (or (magit-toplevel)
      (user-error "Not inside a Git repository")))

(defun magit-gh--check-gh ()
  "Ensure the gh CLI is available."
  (unless (executable-find "gh")
    (user-error "`gh' not found; install from https://cli.github.com")))

(defun magit-gh--fetch-prs ()
  "Fetch open PRs as a list of alists via gh CLI."
  (magit-gh--check-gh)
  (let* ((default-directory (magit-gh--repo-dir))
         (cmd (format "gh pr list --state open --json number,title,author,headRefName --limit %d"
                      magit-gh-pr-limit))
         (output (shell-command-to-string cmd))
         (trimmed (string-trim output)))
    (if (string-prefix-p "[" trimmed)
        (json-parse-string trimmed :array-type 'list :object-type 'alist)
      (user-error "Failed to fetch PRs: %s" trimmed))))

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
            (magit-status-setup-buffer default-directory)
            (message "Checked out PR #%d" number))
        (user-error "Failed to checkout PR #%d; see *magit-gh-output* buffer"
                    number)))))

(defun magit-gh--pr-choices (prs)
  "Build an alist of display strings to PR numbers from PRS."
  (mapcar (lambda (pr)
            (let ((number (alist-get 'number pr))
                  (title (alist-get 'title pr)))
              (cons (format "#%-5d %s" number title)
                    number)))
          prs))

;;; PR List Buffer Commands

(defun magit-gh-pr-list-checkout ()
  "Checkout the PR at point."
  (interactive)
  (if-let ((number (magit-gh--pr-number-at-point)))
      (let ((default-directory magit-gh-pr-list--repo-dir))
        (magit-gh--checkout-pr number))
    (user-error "No PR at point")))

(defun magit-gh-pr-list-browse ()
  "Open the PR at point in the browser."
  (interactive)
  (if-let ((number (magit-gh--pr-number-at-point)))
      (let ((default-directory magit-gh-pr-list--repo-dir))
        (shell-command (format "gh pr view %d --web" number)))
    (user-error "No PR at point")))

(defun magit-gh-pr-list-refresh ()
  "Refresh the PR list buffer."
  (interactive)
  (let ((default-directory magit-gh-pr-list--repo-dir))
    (magit-gh-pr-list)))

;;; Main Commands

(defun magit-gh-pr-list ()
  "List open pull requests for the current repository."
  (interactive)
  (let* ((repo-dir (magit-gh--repo-dir))
         (default-directory repo-dir)
         (prs (magit-gh--fetch-prs))
         (buf (get-buffer-create "*magit-gh: Pull Requests*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null prs)
            (insert (propertize "No open pull requests."
                                'face 'magit-gh-pr-author))
          (insert (propertize (format "%-7s %-60s %-20s %s\n"
                                      "PR" "Title" "Author" "Branch")
                              'face 'magit-gh-header))
          (insert (propertize (make-string 110 ?─) 'face 'magit-gh-header) "\n")
          (dolist (pr prs)
            (let* ((number (alist-get 'number pr))
                   (title (alist-get 'title pr))
                   (author (alist-get 'login (alist-get 'author pr)))
                   (branch (alist-get 'headRefName pr))
                   (title-display (if (> (length title) 58)
                                      (concat (substring title 0 55) "...")
                                    title))
                   (start (point)))
              (insert (propertize (format "%-7s " (format "#%d" number))
                                  'face 'magit-gh-pr-number)
                      (propertize (format "%-60s " title-display)
                                  'face 'magit-gh-pr-title)
                      (propertize (format "%-20s " (or author ""))
                                  'face 'magit-gh-pr-author)
                      (propertize (or branch "")
                                  'face 'magit-gh-pr-branch)
                      "\n")
              (put-text-property start (point) 'magit-gh-pr-number number))))
        (goto-char (point-min))
        (when prs (forward-line 2)))
      (magit-gh-pr-list-mode)
      (setq magit-gh-pr-list--repo-dir repo-dir))
    (pop-to-buffer buf)))

(defun magit-gh-pr-checkout ()
  "Select and checkout a PR using `completing-read'."
  (interactive)
  (let* ((prs (magit-gh--fetch-prs))
         (choices (magit-gh--pr-choices prs))
         (choice (completing-read "Checkout PR: " choices nil t))
         (number (cdr (assoc choice choices))))
    (when number
      (magit-gh--checkout-pr number))))

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

;;; Integration with Magit

(transient-append-suffix 'magit-dispatch "/"
  `(,magit-gh-key "GitHub" magit-gh))

(define-key magit-mode-map (kbd magit-gh-key) #'magit-gh)

(provide 'magit-gh)

;;; magit-gh.el ends here
