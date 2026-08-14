;;; consult-jj-revision.el --- Multi-source revision selection -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (consult "1.0"))

;;; Commentary:

;; Compose bookmark, tag, and commit candidates as Jujutsu revisions.

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'consult-jj-log)
(require 'consult-jj-session)
(require 'consult-jj-tag)

(declare-function consult-jj-new-onto "consult-jj")

(defvar consult-jj-revision-source-bookmark
  '(:name "Bookmark"
    :narrow ?b
    :category consult-jj-bookmark
    :items consult-jj-revision--bookmark-items
    :action consult-jj-revision--new-child)
  "Bookmark source used for revision selection.")

(defvar consult-jj-revision-source-tag
  '(:name "Tag"
    :narrow ?t
    :category consult-jj-tag
    :items consult-jj-revision--tag-items
    :state consult-jj-revision--tag-preview-state
    :action consult-jj-revision--new-child)
  "Tag source used for revision selection.")

(defvar consult-jj-revision-source-commit
  '(:name "Commit"
    :narrow ?c
    :category consult-jj-commit
    :items consult-jj-revision--commit-items
    :state consult-jj-revision--commit-preview-state
    :action consult-jj-revision--new-child)
  "Commit source used for revision selection.")

(defcustom consult-jj-revision-sources
  '(consult-jj-revision-source-bookmark
    consult-jj-revision-source-tag
    consult-jj-revision-source-commit)
  "Ordered Consult sources used for revision selection.
Each symbol names a Consult source plist with an `:items' function.  The
function runs with `default-directory' bound to the selected repository root.
Sources may be omitted, reordered, or extended without advice."
  :type '(repeat symbol)
  :group 'consult-jj)

;;;###autoload
(defun consult-jj-revision ()
  "Select a Jujutsu revision and create a new child commit there."
  (interactive)
  (let* ((root
          (file-name-as-directory
           (expand-file-name (consult-jj--root)))))
    (consult-jj-revision--select
     consult-jj-revision-sources "Jujutsu revisions: " root t))
  nil)

(defun consult-jj-read-revision (&optional prompt root)
  "Read and return one structured Jujutsu revision, or nil.
PROMPT defaults to `Jujutsu revisions: '.  ROOT defaults to the current
project root."
  (setq root
        (file-name-as-directory
         (expand-file-name (or root (consult-jj--root)))))
  (consult-jj-revision--select
   (consult-jj-revision--sources-without-actions)
   (or prompt "Jujutsu revisions: ") root nil))

(defun consult-jj-revision--select (configured-sources prompt root live-p)
  "Read a revision from CONFIGURED-SOURCES under ROOT using PROMPT.
When LIVE-P is non-nil, register one refreshable combined source session."
  (let* ((default-directory root)
         (sources (consult--multi-enabled-sources configured-sources))
         (initial (consult--multi-collection sources))
         (collection
          (if live-p
              (consult-jj--live-candidate-collection
               initial root 'revision nil
               (lambda (refresh-root _tier _source-rev)
                 (let ((default-directory refresh-root))
                   (consult--multi-collection sources)))
               (lambda (candidates _root) candidates))
            initial))
         (selected
          (consult--read
           collection
           :prompt prompt
           :category 'multi-category
           :async-wrap #'consult-jj-revision--async-wrap
           :require-match t
           :sort nil
           :history '(:input consult--line-history)
           :predicate (apply-partially #'consult--multi-predicate sources)
           :annotate (apply-partially #'consult--multi-annotate sources)
           :group (apply-partially #'consult--multi-group sources)
           :lookup (apply-partially #'consult--multi-lookup sources)
           :preview-key (consult--multi-preview-key sources)
           :narrow (consult--multi-narrow sources)
           :state (consult--multi-state sources))))
    (when selected
      (setcar selected (consult-jj-revision--candidate-value (car selected)))
      (when-let* ((history (plist-get (cdr selected) :history)))
        (add-to-history history (car selected)))
      (if (plist-member (cdr selected) :match)
          (when-let* ((new (plist-get (cdr selected) :new)))
            (funcall new (car selected)))
        (when-let* ((action (plist-get (cdr selected) :action)))
          (funcall action (car selected))))
      (car selected))))

(defun consult-jj-revision--async-wrap (async)
  "Wrap revision ASYNC candidates without splitting minibuffer input."
  (consult--async-pipeline
   async
   (consult--async-indicator)
   (consult--async-refresh)))

(defun consult-jj-revision--new-child (revision)
  "Create one new child commit at structured REVISION."
  (consult-jj-new-onto (list revision) nil nil default-directory))

(defun consult-jj-revision--tag-preview-state ()
  "Return a revision-source adapter for the configured tag preview state."
  (consult-jj-revision--preview-state #'consult-jj-tag--preview-state))

(defun consult-jj-revision--commit-preview-state ()
  "Return a revision-source adapter for the configured commit preview state."
  (consult-jj-revision--preview-state #'consult-jj--log-preview-state))

(defun consult-jj-revision--preview-state (constructor)
  "Adapt the preview state from CONSTRUCTOR to revision display candidates."
  (when-let* ((state (funcall constructor)))
    (lambda (action candidate)
      (funcall state action
               (consult-jj-revision--candidate-value candidate)))))

(defun consult-jj-revision--candidate-value (candidate)
  "Return the structured revision represented by CANDIDATE."
  (if (stringp candidate)
      (or (get-text-property 0 'consult-jj-tag candidate)
          (get-text-property 0 'consult-jj-commit candidate)
          candidate)
    candidate))

(defun consult-jj-revision--sources-without-actions ()
  "Return configured revision sources with their default actions removed."
  (mapcar
   (lambda (source)
     (let ((copy
            (copy-sequence
             (if (symbolp source) (symbol-value source) source))))
       (plist-put copy :action nil)))
   consult-jj-revision-sources))

(defun consult-jj-revision--bookmark-items ()
  "Return structured bookmark revision items for `default-directory'."
  (let ((bookmarks
         (funcall consult-jj-bookmark-function default-directory)))
    (cl-mapcar #'cons (consult-jj--bookmark-candidates bookmarks) bookmarks)))

(defun consult-jj-revision--tag-items ()
  "Return tag revision candidates for `default-directory'."
  (let ((tags (funcall consult-jj-tag-function default-directory)))
    (consult-jj-tag--candidates tags)))

(defun consult-jj-revision--commit-items ()
  "Return commit revision candidates for `default-directory'."
  (let ((commits
         (funcall consult-jj-log-function default-directory 'default)))
    (consult-jj--commit-candidates commits)))

(provide 'consult-jj-revision)
;;; consult-jj-revision.el ends here
