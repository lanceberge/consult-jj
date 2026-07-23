;;; consult-jj-jj.el --- Jujutsu command execution for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Internal utils for interfacing with the `jj' executable.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'consult-jj-commit)
(require 'consult-jj-diff)

(defcustom consult-jj-jj-executable "jj"
  "Name of, or path to, the Jujutsu executable."
  :type 'string
  :group 'consult-jj)

(defcustom consult-jj-jj-patch-executable "patch"
  "Name of, or path to, the patch executable used for hunk selection."
  :type 'string
  :group 'consult-jj)

(defconst consult-jj-jj--global-flags '("--no-pager" "--color" "never")
  "Switches passed to every jj invocation so output is plain and parseable.")

;; TODO why did I let claude do this???
(defconst consult-jj-jj--log-template
  (concat
   "concat("
   "\"{\\\"change_id\\\":\", stringify(change_id).escape_json(),"
   "\",\\\"commit_id\\\":\", stringify(commit_id).escape_json(),"
   "\",\\\"description\\\":\", description.escape_json(),"
   "\",\\\"author_name\\\":\", author.name().escape_json(),"
   "\",\\\"author_email\\\":\", stringify(author.email()).escape_json(),"
   "\",\\\"timestamp\\\":\", author.timestamp().format(\"%+\").escape_json(),"
   "\",\\\"bookmarks\\\":\","
   "stringify(local_bookmarks.map(|b| b.name()).join(\"\\0\")).escape_json(),"
   "\",\\\"current\\\":\", current_working_copy,"
   "\",\\\"parent\\\":\", self.contained_in(\"@-\"), \"}\\n\")")
  "Template used to serialize `jj log' commits as JSON lines.")

(defun consult-jj-collect-commits (root)
  "Return structured Jujutsu log commits collected in ROOT.
The configured `revsets.log' remains authoritative because this function does
not pass an explicit revset to `jj log'."
  (mapcar #'consult-jj-jj--parse-commit
          (split-string
           (consult-jj-jj--run root "log" "--no-graph" "--template"
                               consult-jj-jj--log-template)
           "\n" t)))

(defun consult-jj-collect-files (root)
  "Return the list of modified files for `@' in ROOT, relative to ROOT."
  (split-string (consult-jj-jj--run root "diff" "--name-only" "-r" "@") "\n" t))

(defun consult-jj-collect-hunks (root)
  "Return the modified hunks for `@' in ROOT.
`jj diff --git' emits Git-format diffs, so parsing is delegated to
`consult-jj-diff-parse-diff'."
  (consult-jj-diff-parse-diff
   (consult-jj-jj--run root "diff" "--git" "-r" "@") root "@"))

(defun consult-jj-jj--rebase (source destination placement root)
  "Rebase SOURCE at DESTINATION using PLACEMENT under ROOT."
  (let ((flag (alist-get placement
                         '((onto . "--onto")
                           (after . "--insert-after")
                           (before . "--insert-before")))))
    (unless flag
      (error "consult-jj: invalid rebase placement `%s'" placement))
    (consult-jj-jj--run root "rebase" "--source" source flag destination)))

(defun consult-jj-jj--diff-files (files root)
  "Return the Git-format diff for FILES in `@' under ROOT."
  (let ((filesets (consult-jj-jj--exact-filesets files root)))
    (apply #'consult-jj-jj--run root
           (append '("diff" "--git" "-r" "@" "--") filesets))))

(defun consult-jj-jj--restore-hunks (hunks &optional root)
  "Restore HUNKS under ROOT with one Jujutsu restore operation."
  (consult-jj-jj--run-with-hunks
   hunks '("restore" "--changes-in" "@") 'forward root))

(defun consult-jj-jj--squash-hunks (hunks destination &optional root)
  "Squash HUNKS from `@' into DESTINATION under ROOT."
  (consult-jj-jj--run-with-hunks
   hunks (list "squash" "--from" "@" "--into" destination) 'reverse root))

(defun consult-jj-jj--squash-files (files destination root)
  "Squash FILES from `@' into DESTINATION under ROOT."
  (let ((filesets (consult-jj-jj--exact-filesets files root)))
    (apply #'consult-jj-jj--run root
           (append (list "squash" "--from" "@" "--into" destination "--")
                   filesets))))

(defun consult-jj-jj--split-hunks (hunks description root)
  "Split HUNKS from `@' with DESCRIPTION under ROOT."
  (consult-jj-jj--run-with-hunks
   hunks (list "split" "--revision" "@" "--message" description)
   'reverse root))

(defun consult-jj-jj--split-files (files description root)
  "Split FILES from `@' with DESCRIPTION under ROOT."
  (let ((filesets (consult-jj-jj--exact-filesets files root)))
    (apply #'consult-jj-jj--run root
           (append (list "split" "--revision" "@" "--message" description
                         "--")
                   filesets))))

(defun consult-jj-jj--run-with-hunks
    (hunks command-args patch-direction &optional root)
  "Run Jujutsu COMMAND-ARGS with HUNKS selected under ROOT.
PATCH-DIRECTION is `forward' when the command's editor starts at the parent
tree and `reverse' when it starts at the complete changed tree."
  (setq root (or root (consult-jj-hunk-root (car hunks))))
  (unless (memq patch-direction '(forward reverse))
    (error "consult-jj: invalid patch direction `%s'" patch-direction))
  (let ((current-hunks (consult-jj-collect-hunks root)))
    (unless (cl-every (lambda (hunk) (member hunk current-hunks)) hunks)
      (user-error "consult-jj: one or more selected hunks are stale"))
    (dolist (hunk hunks)
      (unless (consult-jj-hunk-supported hunk)
        (error "consult-jj: hunk for `%s' is not patchable (%s)"
               (consult-jj-hunk-preview-path hunk)
               (consult-jj-hunk-status hunk))))
    (let* ((paths (delete-dups
                   (mapcar #'consult-jj-hunk-preview-path hunks)))
           (path-hunks
            (cl-remove-if-not
             (lambda (hunk)
               (member (consult-jj-hunk-preview-path hunk) paths))
             current-hunks))
           (unselected
            (cl-remove-if (lambda (hunk) (member hunk hunks)) path-hunks))
           (filesets (consult-jj-jj--exact-filesets paths root)))
      (if (null unselected)
          (apply #'consult-jj-jj--run root
                 (append command-args '("--") filesets))
        (let* ((patch (consult-jj-hunk->patch unselected))
               (patch-file
                (make-temp-file "consult-jj-selection-" nil ".patch"))
               (edit-args
                (append
                 '("--force")
                 (when (eq patch-direction 'reverse) '("--reverse"))
                 (list "--strip=1" "--directory=$right"
                       (concat "--input=" patch-file)))))
          (unwind-protect
              (progn
                (with-temp-file patch-file
                  (insert patch))
                (apply
                 #'consult-jj-jj--run root
                 (append
                  (list
                   "--config"
                   (format "merge-tools.consult-jj-selection.program=%s"
                           (json-encode-string consult-jj-jj-patch-executable))
                   "--config"
                   (format "merge-tools.consult-jj-selection.edit-args=%s"
                           (json-encode edit-args)))
                  command-args
                  '("--interactive" "--tool" "consult-jj-selection" "--")
                  filesets)))
            (delete-file patch-file)))))))

(defun consult-jj-jj--restore-files (files &optional root)
  "Restore FILES under ROOT from the parent of the working-copy commit."
  (setq root (or root (locate-dominating-file (car files) ".jj")))
  (unless root
    (user-error "consult-jj: no Jujutsu repository found for `%s'" (car files)))
  (setq root (file-name-as-directory (expand-file-name root)))
  (let ((filesets (consult-jj-jj--exact-filesets files root)))
    (apply #'consult-jj-jj--run root "restore" "--" filesets)))

(defun consult-jj-jj--exact-filesets (files root)
  "Return exact Jujutsu filesets for FILES under ROOT."
  (mapcar
   (lambda (file)
     (let ((absolute (expand-file-name file root)))
       (unless (file-in-directory-p absolute root)
         (user-error "consult-jj: `%s' is outside `%s'" file root))
       (concat "root-file:"
               (json-encode-string (file-relative-name absolute root)))))
   files))

(defun consult-jj-jj--parse-commit (line)
  "Parse one JSON log record from LINE into a `consult-jj-commit'."
  (let ((record (json-parse-string line :object-type 'alist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil)))
    (consult-jj-commit-create
     :change-id (alist-get 'change_id record)
     :commit-id (alist-get 'commit_id record)
     :description (alist-get 'description record)
     :author-name (alist-get 'author_name record)
     :author-email (alist-get 'author_email record)
     :timestamp (alist-get 'timestamp record)
     :bookmarks (split-string (alist-get 'bookmarks record) "\0" t)
     :current-p (alist-get 'current record)
     :parent-p (alist-get 'parent record))))

(defun consult-jj-jj--run (root &rest args)
  "Run jj with ARGS in ROOT and return stdout as a string."
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (apply #'process-file consult-jj-jj-executable nil t nil
                           (append consult-jj-jj--global-flags args))))
        (unless (and (integerp status) (zerop status))
          (user-error "consult-jj: jj failed: %s"
                      (string-trim (buffer-string))))
        (buffer-string)))))

(provide 'consult-jj-jj)
;;; consult-jj-jj.el ends here
