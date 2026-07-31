;;; consult-jj-modified-file.el --- Modified-file model for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A `consult-jj-modified-file' is an immutable snapshot of one changed file.

;;; Code:

(require 'cl-lib)

(cl-defstruct (consult-jj-modified-file
               (:constructor consult-jj-modified-file-create)
               (:copier nil))
  "One structured Jujutsu modified-file candidate.

ROOT is the repository root.  BEFORE-PATH and AFTER-PATH are the file paths on
each side of the change.  STATUS is the Jujutsu change status.  HUNKS retains
the file's ordered `consult-jj-hunk' objects.  ADDED and REMOVED are aggregate
changed-line counts.  CONFLICTED-P is non-nil when Jujutsu reports the file as
conflicted in the selected revision."
  root before-path after-path status hunks
  (added 0) (removed 0) conflicted-p)

(provide 'consult-jj-modified-file)
;;; consult-jj-modified-file.el ends here
