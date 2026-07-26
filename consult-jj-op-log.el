;;; consult-jj-op-log.el --- Operation-log discovery for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (consult "1.0"))

;;; Commentary:

;; Structured Jujutsu operation-log discovery, inspection, and recovery.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'consult)
(require 'consult-jj-jj)
(require 'consult-jj-session)

(defvar consult-jj-commit-modified-hook nil
  "Hook run after Consult JJ successfully modifies commit history.")

(defvar consult-jj--commit-modified-root nil
  "Repository root for the current commit-modification notification.")

(cl-defstruct (consult-jj-operation
               (:constructor consult-jj-operation-create)
               (:copier nil))
  "One structured Jujutsu operation-log entry."
  id parent-ids description time-start time-end user workspace attributes
  snapshot-p current-p root-p)

(defcustom consult-jj-op-log-counts '(50 100 200 all)
  "TODO."
  :type '(repeat
          (choice (integer :tag "Operation count")
                  (const :tag "All operations" all)))
  :set
  (lambda (symbol value)
    (unless
        (and
         (consp value)
         (eq (car (last value)) 'all)
         (= (cl-count 'all value) 1)
         (let ((numbers (butlast value)))
           (and numbers
                (cl-every (lambda (number)
                            (and (integerp number) (> number 0)))
                          numbers)
                (cl-loop for (left right) on numbers
                         while right
                         always (< left right)))))
      (error "consult-jj: Operation-log counts must increase and end with `all'"))
    (set-default symbol value))
  :group 'consult-jj)

(defcustom consult-jj-op-log-function #'consult-jj-collect-operations
  "Function used by `consult-jj-op-log' to collect operation-log entries.
The function receives the repository root and a positive count or `all', then
returns a list of `consult-jj-operation' objects."
  :type 'function
  :group 'consult-jj)

(defcustom consult-jj-op-show-buffer-name "*consult-jj-op-show*"
  "Name of the reusable buffer used for showing operations."
  :type 'string
  :group 'consult-jj)

(defcustom consult-jj-op-log-preview-style 'show
  "Preview style used by `consult-jj-read-operation'.
The `show' style previews `jj op show' output.  Nil disables preview."
  :type '(choice (const :tag "Show operation" show)
                 (const :tag "No preview" nil))
  :group 'consult-jj)

(defcustom consult-jj-undo-prefix-arg-confirm t
  "Whether `consult-jj-undo' confirms for a nonnegative prefix argument.
A negative prefix delegates to `consult-jj-redo' and therefore uses
`consult-jj-redo-prefix-arg-confirm'."
  :type 'boolean
  :group 'consult-jj)

(defcustom consult-jj-redo-prefix-arg-confirm t
  "Whether `consult-jj-redo' confirms for a nonnegative prefix argument.
A negative prefix delegates to `consult-jj-undo' and therefore uses
`consult-jj-undo-prefix-arg-confirm'."
  :type 'boolean
  :group 'consult-jj)

(defconst consult-jj-op-log--template
  (concat
   "concat("
   "\"{\\\"operation\\\":\", json(self),"
   "\",\\\"user\\\":\", json(user),"
   "\",\\\"current\\\":\", json(current_operation),"
   "\",\\\"root\\\":\", json(root), \"}\\n\")")
  "Template used to serialize `jj op log' entries as JSON lines.")

(defconst consult-jj-op-log--preview-buffer-name "*consult-jj-op-log-preview*"
  "Base name of the temporary operation preview buffer.")

(defvar consult-jj-op-log--last-count nil
  "Most recent count tier used by an operation-log session.")

(defvar consult-jj-op-log-minibuffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-+") #'consult-jj-op-log-expand)
    (define-key map (kbd "M--") #'consult-jj-op-log-shrink)
    map)
  "Keymap active in `consult-jj-op-log' minibuffers.")

(define-minor-mode consult-jj-op-log-minibuffer-mode
  "Enable operation-log tier commands in the current minibuffer."
  :init-value nil
  :lighter nil
  :keymap consult-jj-op-log-minibuffer-mode-map)

(define-derived-mode consult-jj-op-mode special-mode "Consult-JJ-Operation"
  "Major mode for read-only Jujutsu operation output.")

;;;###autoload
(defun consult-jj-undo (&optional prefix)
  "Undo the last Jujutsu operation.
Without PREFIX, undo one operation without confirmation.  A positive numeric
PREFIX undoes that many operations after confirmation.  Zero and a universal
prefix mean one operation.  A negative prefix delegates to `consult-jj-redo'."
  (interactive "P")
  (let ((count (consult-jj-op-log--prefix-count prefix)))
    (if (< count 0)
        (consult-jj-redo (- count))
      (consult-jj-op-log--run-repeated
       "undo" "Undo" prefix consult-jj-undo-prefix-arg-confirm))))

;;;###autoload
(defun consult-jj-redo (&optional prefix)
  "Redo the most recently undone Jujutsu operation.
Without PREFIX, redo one operation without confirmation.  A positive numeric
PREFIX redoes that many operations after confirmation.  Zero and a universal
prefix mean one operation.  A negative prefix delegates to `consult-jj-undo'."
  (interactive "P")
  (let ((count (consult-jj-op-log--prefix-count prefix)))
    (if (< count 0)
        (consult-jj-undo (- count))
      (consult-jj-op-log--run-repeated
       "redo" "Redo" prefix consult-jj-redo-prefix-arg-confirm))))

(defun consult-jj-op-log--prefix-count (prefix)
  "Return the undo or redo count represented by raw PREFIX."
  (cond
   ((eq prefix '-) -1)
   ((and (integerp prefix) (/= prefix 0)) prefix)
   (t 1)))

(defun consult-jj-op-log--run-repeated (command verb prefix confirm)
  "Run Jujutsu COMMAND for the count represented by PREFIX.
VERB names the operation in the confirmation prompt.  When CONFIRM is
non-nil, an explicit PREFIX requires confirmation."
  (let ((count (consult-jj-op-log--prefix-count prefix)))
    (when (or (null prefix)
              (not confirm)
              (y-or-n-p (format "%s %d entries? " verb count)))
      (let ((root (consult-jj--root))
            modified-p)
        (unwind-protect
            (dotimes (_ count)
              (consult-jj-jj--run root command)
              (setq modified-p t))
          (when modified-p
            (let ((consult-jj--commit-modified-root root))
              (run-hooks 'consult-jj-commit-modified-hook))))))))

(defun consult-jj-collect-operations (root count)
  "Return structured operation-log entries collected in ROOT.
COUNT must be a positive integer or the symbol `all'."
  (unless (or (eq count 'all)
              (and (integerp count) (> count 0)))
    (user-error "consult-jj: Operation-log count must be positive or `all'"))
  (mapcar
   #'consult-jj-op-log--parse-operation
   (split-string
    (apply #'consult-jj-jj--run
           root "op" "log" "--at-op=@" "--ignore-working-copy" "--no-graph"
           (append
            (when (integerp count)
              (list "--limit" (number-to-string count)))
            (list "--template" consult-jj-op-log--template)))
    "\n" t)))

;;;###autoload
(defun consult-jj-op-log (&optional prefix)
  "Select and inspect an entry from the current project's operation log.
Without PREFIX, start at the first configured count tier.  A positive numeric
PREFIX uses that exact count.  A universal PREFIX starts with all operations."
  (interactive "P")
  (let* ((root (consult-jj--root))
         (default-directory root)
         (count
          (cond
           ((eq prefix 'all) 'all)
           ((integerp prefix)
            (if (> prefix 0)
                prefix
              (user-error "consult-jj: Operation-log count must be positive")))
           ((consp prefix) 'all)
           (t (car consult-jj-op-log-counts))))
         (operations (funcall consult-jj-op-log-function root count)))
    (setq consult-jj-op-log--last-count count)
    (if (null operations)
        (message "No Jujutsu operations found.")
      (when-let ((selected
                  (consult-jj-op-log--read operations nil root count)))
        (consult-jj-op-show selected))))
  nil)

;;;###autoload
(defun consult-jj-op-log-expand ()
  "Expand the active or replacing operation log by one count tier."
  (interactive)
  (consult-jj-op-log--move-count 1))

;;;###autoload
(defun consult-jj-op-log-shrink ()
  "Shrink the active or replacing operation log by one count tier."
  (interactive)
  (consult-jj-op-log--move-count -1))

;;;###autoload
(defun consult-jj-op-show (&optional operation)
  "Inspect OPERATION in a reusable read-only buffer.
OPERATION may be a `consult-jj-operation' object or a full operation ID.
Interactively, prompt for an operation when OPERATION is omitted."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root)
         (operation
          (or operation
              (consult-jj-read-operation
               (funcall consult-jj-op-log-function
                        root (car consult-jj-op-log-counts)))))
         (id (consult-jj-op-log--operation-id operation)))
    (when id
      (consult-jj-op-log--display
       (consult-jj-op-log--inspect id root)
       (get-buffer-create consult-jj-op-show-buffer-name)
       root))))

(defun consult-jj-read-operation (operations &optional prompt)
  "Read and return one structured operation from OPERATIONS, or nil.
Completion candidates show only operation descriptions.  PROMPT defaults to
`Jujutsu operations: '."
  (consult-jj-op-log--read operations prompt nil nil))

(defun consult-jj-op-log--read (operations prompt live-root count)
  "Read one of OPERATIONS using PROMPT, or return nil.
When LIVE-ROOT is non-nil, register a refreshable operation-log session there.
COUNT is the active tier retained by that live session."
  (let ((candidates (consult-jj-op-log--candidates operations)))
    (when candidates
      (let ((read
             (lambda ()
               (consult--read
                (if live-root
                    (consult-jj--live-candidate-collection
                     candidates live-root 'op-log count
                     #'consult-jj-op-log--collect-session-operations
                     #'consult-jj-op-log--present-session-operations)
                  candidates)
                :prompt (or prompt "Jujutsu operations: ")
                :category 'consult-jj-operation
                :require-match t
                :sort nil
                :lookup #'consult-jj-op-log--lookup-operation
                :predicate #'consult-jj-op-log--narrow-p
                :narrow '((?s . "Snapshot") (?o . "Other"))
                :history '(:input consult--line-history)
                :state (consult-jj-op-log--preview-state)))))
        (if live-root
            (minibuffer-with-setup-hook
                #'consult-jj-op-log-minibuffer-mode
              (funcall read))
          (funcall read))))))

(defun consult-jj-op-log--move-count (step)
  "Move the current operation-log count by STEP, which must be 1 or -1."
  (let* ((session
          (cl-find-if
           (lambda (candidate-session)
             (and
              (eq (consult-jj--candidate-session-view candidate-session)
                  'op-log)
              (eq (consult-jj--candidate-session-buffer candidate-session)
                  (current-buffer))))
           consult-jj--candidate-sessions))
         (current
          (if session
              (consult-jj--candidate-session-tier session)
            consult-jj-op-log--last-count))
         (next (and current
                    (consult-jj-op-log--adjacent-count current step))))
    (unless current
      (user-error "consult-jj: No operation-log tier to %s"
                  (if (= step 1) "expand" "shrink")))
    (if (null next)
        (message "consult-jj: Operation log is already at the %s tier"
                 (if (= step 1) "broadest" "narrowest"))
      (setq consult-jj-op-log--last-count next)
      (if session
          (progn
            (setf (consult-jj--candidate-session-tier session) next)
            (funcall
             (consult-jj--candidate-session-replace session)
             (consult-jj-op-log--candidates
              (funcall
               consult-jj-op-log-function
               (consult-jj--candidate-session-root session)
               next))))
        (consult-jj-op-log next))))
  nil)

(defun consult-jj-op-log--adjacent-count (current step)
  "Return the configured count adjacent to CURRENT in direction STEP."
  (let ((numbers (cl-remove-if-not #'integerp consult-jj-op-log-counts)))
    (if (= step 1)
        (or (cl-find-if (lambda (count)
                          (and (integerp current) (> count current)))
                        numbers)
            (and (not (eq current 'all))
                 (memq 'all consult-jj-op-log-counts)
                 'all))
      (car
       (last
        (cl-remove-if-not
         (lambda (count)
           (or (eq current 'all)
               (and (integerp current) (< count current))))
         numbers))))))

(defun consult-jj-op-log--operation-id (operation)
  "Return the full ID represented by OPERATION."
  (cond
   ((consult-jj-operation-p operation)
    (consult-jj-operation-id operation))
   ((stringp operation) operation)
   ((null operation) nil)
   (t (user-error "consult-jj: Invalid Jujutsu operation `%S'" operation))))

(defun consult-jj-op-log--display (output buffer root)
  "Render operation OUTPUT in BUFFER under ROOT and display it."
  (consult-jj-op-log--render output buffer root)
  (display-buffer buffer)
  buffer)

(defun consult-jj-op-log--inspect (operation-id root)
  "Return read-only inspection output for OPERATION-ID under ROOT."
  (consult-jj-jj--run
   root "op" "show" "--at-op=@" "--ignore-working-copy" operation-id))

(defun consult-jj-op-log--render (output buffer root)
  "Render operation OUTPUT in BUFFER under ROOT."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert output))
    (setq default-directory root)
    (consult-jj-op-mode)
    (goto-char (point-min)))
  buffer)

(defun consult-jj-op-log--preview-state ()
  "Return the preview state selected by `consult-jj-op-log-preview-style'."
  (pcase consult-jj-op-log-preview-style
    ('show
     (let ((preview (consult--buffer-preview))
           (root default-directory)
           buffer)
       (lambda (action operation)
         (when (and (eq action 'preview) operation)
           (unless (buffer-live-p buffer)
             (setq buffer
                   (generate-new-buffer consult-jj-op-log--preview-buffer-name)))
           (consult-jj-op-log--render
            (consult-jj-op-log--inspect
             (consult-jj-op-log--operation-id operation) root)
            buffer root))
         (funcall preview action
                  (and (eq action 'preview) operation buffer))
         (when (and (memq action '(exit return)) (buffer-live-p buffer))
           (kill-buffer buffer)))))
    ('nil nil)
    (style
     (user-error "consult-jj: Invalid operation-log preview style `%s'" style))))

(defun consult-jj-op-log--parse-operation (line)
  "Parse one JSON operation-log record from LINE."
  (let* ((record (json-parse-string line :object-type 'alist
                                    :array-type 'list
                                    :null-object nil
                                    :false-object nil))
         (operation (alist-get 'operation record))
         (time (alist-get 'time operation)))
    (consult-jj-operation-create
     :id (alist-get 'id operation)
     :parent-ids (alist-get 'parents operation)
     :description (alist-get 'description operation)
     :time-start (alist-get 'start time)
     :time-end (alist-get 'end time)
     :user (alist-get 'user record)
     :workspace (alist-get 'workspace_name operation)
     :attributes (alist-get 'attributes operation)
     :snapshot-p (alist-get 'is_snapshot operation)
     :current-p (alist-get 'current record)
     :root-p (alist-get 'root record))))

(defun consult-jj-op-log--collect-session-operations (root count)
  "Collect operation-log entries under ROOT for COUNT."
  (funcall consult-jj-op-log-function
           root (or count (car consult-jj-op-log-counts))))

(defun consult-jj-op-log--present-session-operations (operations _root)
  "Present OPERATIONS as completion candidates."
  (consult-jj-op-log--candidates operations))

(consult-jj--register-candidate-session-adapter
 'op-log
 #'consult-jj-op-log--collect-session-operations
 #'consult-jj-op-log--present-session-operations)

(defun consult-jj-op-log--candidates (operations)
  "Build completion candidates for OPERATIONS in source order."
  (cl-loop for operation in operations
           for index from 0
           collect (consult-jj-op-log--candidate operation index)))

(defun consult-jj-op-log--candidate (operation index)
  "Build a completion candidate for OPERATION disambiguated by INDEX."
  (let* ((description (consult-jj-operation-description operation))
         (display (if (string-empty-p (or description ""))
                      "(no operation description)"
                    description))
         (candidate (consult--tofu-append display index)))
    (add-text-properties
     0 1
     (list 'consult-jj-operation operation
           'consult-jj-op-log-kind
           (if (consult-jj-operation-snapshot-p operation) ?s ?o))
     candidate)
    candidate))

(defun consult-jj-op-log--lookup-operation (selected candidates &rest _)
  "Return the structured operation for SELECTED from CANDIDATES."
  (when-let ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-operation candidate)))

(defun consult-jj-op-log--narrow-p (candidate)
  "Return non-nil when CANDIDATE is visible under the active narrow."
  (or (null consult--narrow)
      (eq (get-text-property 0 'consult-jj-op-log-kind candidate)
          consult--narrow)))

(provide 'consult-jj-op-log)
;;; consult-jj-op-log.el ends here
