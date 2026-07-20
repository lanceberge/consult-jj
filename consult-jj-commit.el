;;; consult-jj-commit.el --- Commit model for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Establish a struct for jj commits.

;;; Code:

(require 'cl-lib)

(cl-defstruct (consult-jj-commit
               (:constructor consult-jj-commit-create)
               (:copier nil))
  "One Jujutsu commit candidate.

CHANGE-ID and COMMIT-ID are full identifiers.  DESCRIPTION is retained
verbatim, including multiple lines.  AUTHOR-NAME, AUTHOR-EMAIL, and TIMESTAMP
describe authorship.  BOOKMARKS is a list of local bookmark names.  CURRENT-P
is non-nil for `@', and PARENT-P is non-nil for commits in `@-'."
  change-id
  commit-id
  description
  author-name
  author-email
  timestamp
  bookmarks
  current-p
  parent-p)

(provide 'consult-jj-commit)
;;; consult-jj-commit.el ends here
