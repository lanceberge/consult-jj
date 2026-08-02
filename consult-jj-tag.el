;;; consult-jj-tag.el --- Tag discovery for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Structured local and remote Jujutsu tag discovery.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'consult)
(require 'consult-jj-commit)
(require 'consult-jj-jj)
(require 'consult-jj-log)
(require 'consult-jj-session)

(cl-defstruct (consult-jj-tag
               (:constructor consult-jj-tag-create)
               (:copier nil))
  "One Jujutsu tag candidate.

NAME is the local name portion.  REMOTE is nil for a local tag and the exact
remote name otherwise.  REVISION is the exact revision accepted by Jujutsu.
TARGET preserves Jujutsu's serialized target data.  TARGET-COMMIT is the fully
populated normal target commit when one exists.  CONFLICTED-P is non-nil when
TARGET is conflicted.  TARGETLESS-P is non-nil when the tag has no target."
  name
  remote
  revision
  target
  target-commit
  conflicted-p
  targetless-p)

(defcustom consult-jj-tag-preview-style 'diff
  "Preview style used by `consult-jj-read-tag'.
The `diff' style uses the commit-log preview for a normal target.  The `none'
style disables preview."
  :type '(choice (const :tag "Diff" diff)
                 (const :tag "None" none))
  :group 'consult-jj)

(defcustom consult-jj-tag-function #'consult-jj-collect-tags
  "Function used by `consult-jj-tag' to collect tag candidates.
The function receives the repository root and must return a list of
`consult-jj-tag' objects."
  :type 'function
  :group 'consult-jj)

(defconst consult-jj-tag--template
  (concat
   "concat("
   "\"{\\\"tag\\\":\", json(self),"
   "\",\\\"conflicted\\\":\", json(self.conflict()),"
   "\",\\\"normal_target\\\":\","
   "if(self.normal_target(), "
   (consult-jj-jj--commit-record-template "self.normal_target()")
   ","
   "\"null\"), \"}\\n\")")
  "Template used to serialize tag records and normal target commits.")

(defun consult-jj-collect-tags (root)
  "Return visible structured local and remote Jujutsu tags in ROOT."
  (let ((tags
         (mapcar
          #'consult-jj-tag--parse
          (split-string
           (consult-jj-jj--run
            root "tag" "list" "--quiet" "--template" consult-jj-tag--template)
           "\n" t))))
    (if consult-jj-show-git-bookmarks
        tags
      (cl-remove "git" tags
                 :key #'consult-jj-tag-remote
                 :test #'equal))))

;;;###autoload
(defun consult-jj-tag ()
  "Select a Jujutsu tag and create a new child commit at its target."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root)
         (tags (funcall consult-jj-tag-function root)))
    (if (null tags)
        (message "No Jujutsu tags found.")
      (when-let* ((selected (consult-jj-tag--read tags nil root)))
        (cond
         ((consult-jj-tag-conflicted-p selected)
          (user-error "consult-jj: Tag `%s' target is conflicted"
                      (consult-jj-tag-revision selected)))
         ((consult-jj-tag-targetless-p selected)
          (user-error "consult-jj: Tag `%s' target is targetless or deleted"
                      (consult-jj-tag-revision selected)))
         ((null (consult-jj-tag-target-commit selected))
          (user-error "consult-jj: Tag `%s' does not have one normal target"
                      (consult-jj-tag-revision selected))))
        (consult-jj-new-here
         (consult-jj-tag-revision selected) nil nil root))))
  nil)

(defun consult-jj-read-tag (tags &optional prompt)
  "Read and return one structured tag from TAGS, or nil.
PROMPT defaults to `Jujutsu tags: '."
  (consult-jj-tag--read tags prompt nil))

(defun consult-jj-tag--read (tags prompt live-root)
  "Read one of TAGS using PROMPT, or return nil.
When LIVE-ROOT is non-nil, register a refreshable tag session there."
  (let ((candidates (consult-jj-tag--candidates tags))
        (state (consult-jj-tag--preview-state)))
    (when candidates
      (consult--read
       (if live-root
           (consult-jj--live-candidate-collection
            candidates live-root 'tag nil
            #'consult-jj-tag--collect-session-tags
            #'consult-jj-tag--present-session-tags)
         candidates)
       :prompt (or prompt "Jujutsu tags: ")
       :category 'consult-jj-tag
       :require-match t
       :sort nil
       :lookup #'consult-jj-tag--lookup
       :state state
       :history '(:input consult--line-history)))))

(defun consult-jj-tag--parse (line)
  "Parse one JSON tag record from LINE into a tag object."
  (let* ((serialized
          (json-parse-string line :object-type 'alist
                            :array-type 'list
                            :null-object nil
                            :false-object nil))
         (record (alist-get 'tag serialized))
         (normal-target (alist-get 'normal_target serialized))
         (name (alist-get 'name record))
         (remote (alist-get 'remote record))
         (target (alist-get 'target record))
         (conflicted-p (alist-get 'conflicted serialized)))
    (consult-jj-tag-create
     :name name
     :remote remote
     :revision (if remote (concat name "@" remote) name)
     :target target
     :target-commit
     (and normal-target
          (consult-jj-jj--commit-from-record normal-target))
     :conflicted-p conflicted-p
     :targetless-p (and (null target) (not conflicted-p)))))

(defun consult-jj-tag--candidates (tags)
  "Build completion candidates for structured TAGS in source order."
  (cl-loop for tag in tags
           for index from 0
           collect
           (let ((candidate
                  (consult--tofu-append
                   (consult-jj-tag-revision tag) index)))
             (add-text-properties 0 1 (list 'consult-jj-tag tag) candidate)
             candidate)))

(defun consult-jj-tag--lookup (selected candidates &rest _)
  "Return the tag object for SELECTED from CANDIDATES."
  (when-let* ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-tag candidate)))

(defun consult-jj-tag--collect-session-tags (root _tier _source-rev)
  "Collect structured tags under ROOT for a live session."
  (funcall consult-jj-tag-function root))

(defun consult-jj-tag--present-session-tags (tags _root)
  "Present structured TAGS as completion candidates."
  (consult-jj-tag--candidates tags))

(consult-jj--register-candidate-session-adapter
 'tag
 #'consult-jj-tag--collect-session-tags
 #'consult-jj-tag--present-session-tags)

(defun consult-jj-tag--preview-state ()
  "Return the preview state selected by `consult-jj-tag-preview-style'."
  (pcase consult-jj-tag-preview-style
    ('diff
     (let ((preview (consult-jj-log--diff-preview-state)))
       (lambda (action tag)
         (funcall preview action
                  (and tag (consult-jj-tag-target-commit tag))))))
    ('none nil)
    (style
     (user-error "consult-jj: Invalid tag preview style `%s'" style))))

(provide 'consult-jj-tag)
;;; consult-jj-tag.el ends here
