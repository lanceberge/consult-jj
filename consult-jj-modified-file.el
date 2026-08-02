;;; consult-jj-modified-file.el --- Modified-file model for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A `consult-jj-modified-file' is an immutable snapshot of one changed file.

;;; Code:

(require 'cl-lib)

(cl-defstruct (consult-jj-modified-file
               (:constructor consult-jj-modified-file--create)
               (:copier nil))
  "One structured Jujutsu modified-file candidate.

SOURCE-REV identifies the revision the file came from and defaults to `@'.
BEFORE-PATH and AFTER-PATH are the file paths on each side of the change.
STATUS is the Jujutsu change status.  HUNKS retains the file's ordered
`consult-jj-hunk' objects.  ADDED and REMOVED are aggregate changed-line
counts.  CONFLICTED-P is non-nil when Jujutsu reports the file as conflicted
in the selected revision."
  (source-rev "@") before-path after-path status hunks
  (added 0) (removed 0) conflicted-p)

(defun consult-jj-modified-file-create (&rest arguments)
  "Create a modified file from keyword ARGUMENTS.
An omitted or nil `:source-rev' is normalized to `@'.  The obsolete `:root'
keyword is accepted but ignored so repository context is never retained in
the target."
  (setq arguments (copy-sequence arguments))
  (cl-remf arguments :root)
  (unless (plist-get arguments :source-rev)
    (setq arguments (plist-put arguments :source-rev "@")))
  (apply #'consult-jj-modified-file--create arguments))

(provide 'consult-jj-modified-file)
;;; consult-jj-modified-file.el ends here
