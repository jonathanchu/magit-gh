;;; magit-gh-utils.el --- Shared infrastructure for magit-gh -*- lexical-binding: t -*-

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

;; Shared infrastructure for magit-gh: the customization group, faces,
;; common gh helpers (`magit-gh--check-gh', `magit-gh--repo-dir',
;; `magit-gh--async-fetch', `magit-gh--format-age'), and the buffer
;; navigation commands.  This file has no dependencies on the other
;; magit-gh modules, which require it in turn.

;;; Code:

(require 'magit)
(require 'iso8601)

;;; Custom Group

(defgroup magit-gh nil
  "GitHub CLI integration for Magit."
  :prefix "magit-gh-"
  :group 'magit-extensions)

;;; Custom Faces

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

(defvar-local magit-gh--navigation-property 'magit-gh-pr-number
  "Text property identifying navigable item rows in the current buffer.
`magit-gh--next-item' and `magit-gh--previous-item' move between
lines carrying this property.  Each major mode sets it to the
property its rows are tagged with.")

(defun magit-gh--next-item ()
  "Move point to the next item row."
  (interactive)
  (let ((start (point))
        (prop magit-gh--navigation-property))
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
        (prop magit-gh--navigation-property))
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

(defun magit-gh--async-fetch (cmd dir callback &optional errback)
  "Run CMD asynchronously in DIR, parse JSON output, and call CALLBACK.
CMD is a shell command string (typically a gh CLI invocation).
DIR is the working directory in which CMD runs; passing it
explicitly keeps the process directory independent of whichever
buffer happens to be current when the process starts.
CALLBACK is called with the parsed JSON data on success.
ERRBACK is called with an error message on failure; if nil,
a message is displayed instead."
  (let* ((default-directory dir)
         (output (list ""))
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

(provide 'magit-gh-utils)

;;; magit-gh-utils.el ends here
