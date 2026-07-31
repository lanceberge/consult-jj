;;; consult-jj-bookmark.el --- Bookmark model for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Establish a struct for local and remote Jujutsu bookmarks.

;;; Code:

(require 'cl-lib)

(cl-defstruct (consult-jj-bookmark
               (:constructor consult-jj-bookmark-create)
               (:copier nil))
  "One Jujutsu bookmark candidate.

NAME is the local name portion.  REMOTE is nil for a local bookmark and the
exact remote name otherwise.  REVISION is the exact revision accepted by
Jujutsu.  TARGET preserves Jujutsu's serialized target data, including
conflicted targets.  TARGET-COMMIT is the fully populated normal target commit
when one exists.  CONFLICTED-P is non-nil when TARGET is conflicted."
  name
  remote
  revision
  target
  target-commit
  conflicted-p)

(provide 'consult-jj-bookmark)
;;; consult-jj-bookmark.el ends here
