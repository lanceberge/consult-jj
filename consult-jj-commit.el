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
is non-nil for `@', and PARENT-P is non-nil for commits in `@-'.

SHORT-CHANGE-ID, SHORT-COMMIT-ID, and CHANGE-OFFSET retain Jujutsu-formatted
identity.  REMOTE-BOOKMARKS and TAGS contain Jujutsu's non-redundant formatted
reference names.  WORKING-COPY-WORKSPACES names every workspace at this
commit.  DIVERGENT-P, EMPTY-P, and CONFLICTED-P retain commit state."
  change-id
  commit-id
  description
  author-name
  author-email
  timestamp
  bookmarks
  current-p
  parent-p
  short-change-id
  short-commit-id
  change-offset
  remote-bookmarks
  tags
  working-copy-workspaces
  divergent-p
  empty-p
  conflicted-p)

(provide 'consult-jj-commit)
;;; consult-jj-commit.el ends here
