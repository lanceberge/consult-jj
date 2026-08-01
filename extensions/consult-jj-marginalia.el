;;; consult-jj-marginalia.el --- Marginalia integration for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (marginalia "1.0"))
;; Version: 0.1.0

;;; Commentary:

;; Add optional Marginalia annotations for Consult JJ completion candidates.

;;; Code:

(require 'subr-x)
(require 'marginalia)
(require 'consult-jj)

(defcustom consult-jj-marginalia-file-annotations
  '(consult-jj-marginalia-file-conflict
    consult-jj-marginalia-file-line-counts
    consult-jj-marginalia-file-rename-source)
  "Ordered field functions used to annotate modified-file candidates.
Each function receives a `consult-jj-modified-file' object and the original
completion candidate.  It returns one styled string, or nil to omit the field."
  :type '(repeat function)
  :group 'consult-jj)

(defcustom consult-jj-marginalia-hunk-annotations
  '(consult-jj-marginalia-hunk-conflict
    consult-jj-marginalia-hunk-line-counts)
  "Ordered field functions used to annotate modified-hunk candidates.
Each function receives a `consult-jj-hunk' object and the original completion
candidate.  It returns one styled string, or nil to omit the field."
  :type '(repeat function)
  :group 'consult-jj)

(defcustom consult-jj-marginalia-commit-annotations
  '(consult-jj-marginalia-commit-change-identity
    consult-jj-marginalia-commit-bookmarks
    consult-jj-marginalia-commit-tags
    consult-jj-marginalia-commit-workspaces
    consult-jj-marginalia-commit-state
    consult-jj-marginalia-commit-author
    consult-jj-marginalia-commit-timestamp)
  "Ordered field functions used to annotate commit candidates.
Each function receives a `consult-jj-commit' object and the original
completion candidate.  It returns one styled string, or nil to omit the field."
  :type '(repeat function)
  :group 'consult-jj)

(defvar consult-jj-marginalia--saved-entries nil
  "Marginalia annotator registry saved before enabling the integration.")

(defvar consult-jj-marginalia--installed-p nil
  "Non-nil while the mode owns its Marginalia registry entries.")

(defvar consult-jj-marginalia--file-annotations nil
  "Modified-file field functions captured when the mode was enabled.")

(defvar consult-jj-marginalia--hunk-annotations nil
  "Modified-hunk field functions captured when the mode was enabled.")

(defvar consult-jj-marginalia--commit-annotations nil
  "Commit field functions captured when the mode was enabled.")

;;;###autoload
(define-minor-mode consult-jj-marginalia-mode
  "Toggle global Marginalia annotations for Consult JJ candidates."
  :global t
  :group 'consult-jj
  (if consult-jj-marginalia-mode
      (consult-jj-marginalia--enable)
    (consult-jj-marginalia--disable)))

(defun consult-jj-marginalia-file-conflict (file _candidate)
  "Return FILE's conflict field, or nil.
CANDIDATE is the original completion display string."
  (consult-jj-marginalia--conflict
   (consult-jj-modified-file-conflicted-p file)))

(defun consult-jj-marginalia-file-line-counts (file _candidate)
  "Return FILE's added and removed line-count field, or nil.
CANDIDATE is the original completion display string."
  (consult-jj-marginalia--line-counts
   (consult-jj-modified-file-added file)
   (consult-jj-modified-file-removed file)))

(defun consult-jj-marginalia-file-rename-source (file _candidate)
  "Return FILE's source-path field when it is a rename, or nil.
CANDIDATE is the original completion display string."
  (let ((before (consult-jj-modified-file-before-path file))
        (after (consult-jj-modified-file-after-path file)))
    (when (and (eq (consult-jj-modified-file-status file) 'renamed)
               before
               (not (equal before after)))
      (propertize
       (format "from %s" before)
       'face 'marginalia-file-name))))

(defun consult-jj-marginalia-file-status (file _candidate)
  "Return FILE's Jujutsu status field, or nil.
CANDIDATE is the original completion display string."
  (when-let ((status (consult-jj-modified-file-status file)))
    (propertize (symbol-name status) 'face 'marginalia-type)))

(defun consult-jj-marginalia-file-hunk-count (file _candidate)
  "Return FILE's modified-hunk count field, or nil.
CANDIDATE is the original completion display string."
  (when-let ((hunks (consult-jj-modified-file-hunks file)))
    (let ((count (length hunks)))
      (propertize
       (format "%d %s" count (if (= count 1) "hunk" "hunks"))
       'face 'marginalia-number))))

(defun consult-jj-marginalia-hunk-conflict (hunk _candidate)
  "Return HUNK's containing-file conflict field, or nil.
CANDIDATE is the original completion display string."
  (consult-jj-marginalia--conflict
   (consult-jj-hunk-conflicted-p hunk)))

(defun consult-jj-marginalia-hunk-line-counts (hunk _candidate)
  "Return HUNK's added and removed line-count field, or nil.
CANDIDATE is the original completion display string."
  (consult-jj-marginalia--line-counts
   (consult-jj-hunk-added hunk)
   (consult-jj-hunk-removed hunk)))

(defun consult-jj-marginalia-hunk-status (hunk _candidate)
  "Return HUNK's Jujutsu status field, or nil.
CANDIDATE is the original completion display string."
  (when-let ((status (consult-jj-hunk-status hunk)))
    (propertize (symbol-name status) 'face 'marginalia-type)))

(defun consult-jj-marginalia-hunk-unsupported-shape (hunk _candidate)
  "Return an unsupported-shape field for HUNK, or nil.
CANDIDATE is the original completion display string."
  (when (and (consult-jj-hunk-status hunk)
             (not (consult-jj-hunk-supported hunk)))
    (propertize "unsupported" 'face 'marginalia-modified)))

(defun consult-jj-marginalia-commit-change-identity (commit _candidate)
  "Return COMMIT's shortest change identity and working-copy context.
CANDIDATE is the original completion display string."
  (when-let ((change-id (consult-jj-commit-short-change-id commit)))
    (propertize
     (concat
      (cond
       ((consult-jj-commit-current-p commit) "@ ")
       ((consult-jj-commit-parent-p commit) "@- ")
       (t ""))
      change-id
      (if (and (consult-jj-commit-divergent-p commit)
               (numberp (consult-jj-commit-change-offset commit)))
          (format "/%d" (consult-jj-commit-change-offset commit))
        ""))
     'face 'marginalia-key)))

(defun consult-jj-marginalia-commit-bookmarks (commit _candidate)
  "Return COMMIT's captured local and non-redundant remote bookmarks.
CANDIDATE is the original completion display string."
  (when-let ((bookmarks
              (append
               (consult-jj-commit-bookmarks commit)
               (consult-jj-commit-remote-bookmarks commit))))
    (propertize
     (string-join bookmarks " ")
     'face 'marginalia-value)))

(defun consult-jj-marginalia-commit-tags (commit _candidate)
  "Return COMMIT's captured non-redundant tag names, or nil.
CANDIDATE is the original completion display string."
  (when-let ((tags (consult-jj-commit-tags commit)))
    (propertize
     (string-join tags " ")
     'face 'marginalia-value)))

(defun consult-jj-marginalia-commit-workspaces (commit _candidate)
  "Return every workspace whose working-copy commit is COMMIT.
CANDIDATE is the original completion display string."
  (when-let ((workspaces
              (consult-jj-commit-working-copy-workspaces commit)))
    (propertize
     (string-join
      (mapcar (lambda (workspace) (concat workspace "@")) workspaces)
      " ")
     'face 'marginalia-value)))

(defun consult-jj-marginalia-commit-state (commit _candidate)
  "Return COMMIT's captured divergent, empty, and conflicted state.
CANDIDATE is the original completion display string."
  (when-let ((states
              (delq
               nil
               (list
                (and (consult-jj-commit-divergent-p commit) "divergent")
                (and (consult-jj-commit-empty-p commit) "empty")
                (and (consult-jj-commit-conflicted-p commit) "conflicted")))))
    (propertize
     (string-join states " ")
     'face 'marginalia-modified)))

(defun consult-jj-marginalia-commit-author (commit _candidate)
  "Return COMMIT's captured author with full identity in help text.
CANDIDATE is the original completion display string."
  (let ((name (consult-jj-commit-author-name commit))
        (email (consult-jj-commit-author-email commit)))
    (when-let ((display
                (cond
                 ((and name (not (string-empty-p name))) name)
                 ((and email (not (string-empty-p email))) email))))
      (propertize
       display
       'face 'marginalia-documentation
       'help-echo
       (if (and name (not (string-empty-p name))
                email (not (string-empty-p email)))
           (format "%s <%s>" name email)
         display)))))

(defun consult-jj-marginalia-commit-timestamp (commit _candidate)
  "Return COMMIT's timestamp using Marginalia's age convention.
CANDIDATE is the original completion display string."
  (when-let ((timestamp (consult-jj-commit-timestamp commit)))
    (marginalia--time (date-to-time timestamp))))

(defun consult-jj-marginalia-commit-id (commit _candidate)
  "Return COMMIT's shortest captured commit ID, or nil.
CANDIDATE is the original completion display string."
  (when-let ((commit-id (consult-jj-commit-short-commit-id commit)))
    (propertize commit-id 'face 'marginalia-value)))

(defun consult-jj-marginalia--enable ()
  "Install Marginalia annotators for Consult JJ candidate categories."
  (setq consult-jj-marginalia--saved-entries
        marginalia-annotators
        consult-jj-marginalia--installed-p t
        consult-jj-marginalia--file-annotations
        (copy-sequence consult-jj-marginalia-file-annotations)
        consult-jj-marginalia--hunk-annotations
        (copy-sequence consult-jj-marginalia-hunk-annotations)
        consult-jj-marginalia--commit-annotations
        (copy-sequence consult-jj-marginalia-commit-annotations))
  (setq marginalia-annotators
        (consult-jj-marginalia--replace-annotator
         'consult-jj-modified-file
         #'consult-jj-marginalia--annotate-file))
  (setq marginalia-annotators
        (consult-jj-marginalia--replace-annotator
         'consult-jj-modified-hunk
         #'consult-jj-marginalia--annotate-hunk))
  (setq marginalia-annotators
        (consult-jj-marginalia--replace-annotator
         'consult-jj-commit
         #'consult-jj-marginalia--annotate-commit)))

(defun consult-jj-marginalia--disable ()
  "Restore the Marginalia registry saved before the mode was enabled."
  (when consult-jj-marginalia--installed-p
    (setq marginalia-annotators
          consult-jj-marginalia--saved-entries))
  (setq consult-jj-marginalia--saved-entries nil
        consult-jj-marginalia--installed-p nil
        consult-jj-marginalia--file-annotations nil
        consult-jj-marginalia--hunk-annotations nil
        consult-jj-marginalia--commit-annotations nil))

(defun consult-jj-marginalia--annotate-file (candidate)
  "Annotate modified-file CANDIDATE using the captured field functions."
  (consult-jj-marginalia--annotation
   (get-text-property 0 'consult-jj-modified-file candidate)
   candidate
   consult-jj-marginalia--file-annotations))

(defun consult-jj-marginalia--annotate-hunk (candidate)
  "Annotate modified-hunk CANDIDATE using the captured field functions."
  (consult-jj-marginalia--annotation
   (get-text-property 0 'consult-jj-hunk candidate)
   candidate
   consult-jj-marginalia--hunk-annotations))

(defun consult-jj-marginalia--annotate-commit (candidate)
  "Annotate commit CANDIDATE using the captured field functions."
  (consult-jj-marginalia--annotation
   (get-text-property 0 'consult-jj-commit candidate)
   candidate
   consult-jj-marginalia--commit-annotations))

(defun consult-jj-marginalia--annotation (object candidate functions)
  "Compose FUNCTIONS for OBJECT and original CANDIDATE as one annotation."
  (when object
    (when-let ((fields
                (delq
                 nil
                 (mapcar
                  (lambda (function)
                    (funcall function object candidate))
                  functions))))
      (concat
       (propertize " " 'marginalia--align t)
       marginalia-separator
       (string-join fields marginalia-separator)))))

(defun consult-jj-marginalia--replace-annotator (category annotator)
  "Return the registry with CATEGORY replaced by ANNOTATOR.
Entries for every other category retain their identity."
  (if (assq category marginalia-annotators)
      (mapcar
       (lambda (entry)
         (if (eq (car entry) category)
             (list category annotator)
           entry))
       marginalia-annotators)
    (cons (list category annotator) marginalia-annotators)))

(defun consult-jj-marginalia--conflict (conflicted-p)
  "Return a conflict field when CONFLICTED-P is non-nil."
  (when conflicted-p
    (propertize "conflicted" 'face 'marginalia-modified)))

(defun consult-jj-marginalia--line-counts (added removed)
  "Return a styled line-count field for ADDED and REMOVED, or nil."
  (when (and (numberp added) (numberp removed))
    (propertize
     (format "+%d -%d" added removed)
     'face 'marginalia-number)))

(provide 'consult-jj-marginalia)
;;; consult-jj-marginalia.el ends here
