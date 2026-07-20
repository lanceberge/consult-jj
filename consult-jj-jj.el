;;; consult-jj-jj.el --- Jujutsu command execution for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Internal utils for interfacing with the `jj' executable.

;;; Code:

(require 'subr-x)
(require 'json)
(require 'consult-jj-commit)
(require 'consult-jj-diff)

(defcustom consult-jj-jj-executable "jj"
  "Name of, or path to, the Jujutsu executable."
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
