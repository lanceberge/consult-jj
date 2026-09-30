;;; consult-jj-rebase.el --- Multi-commit rebase execution -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'consult-jj-commit)
(require 'consult-jj-jj)

(cl-defstruct consult-jj--rebase-plan
  identities destination placement arrangement root)

(cl-defstruct consult-jj--rebase-result
  attempted failure changed operation-error hook-error)

(defun consult-jj--perform-multiple-rebase
    (sources destination placement root arrangement)
  (let* ((plan (consult-jj--prepare-rebase-plan
                sources destination placement root arrangement))
         (consult-jj--commit-modified-root
          (file-name-as-directory (expand-file-name root)))
         (operation-before (consult-jj--rebase-operation-id root))
         (result (consult-jj--execute-rebase-plan plan)))
    (consult-jj--notify-rebase-result result operation-before root)
    (consult-jj--report-rebase-result result)))

(defun consult-jj--prepare-rebase-plan
    (sources destination placement root arrangement)
  (unless (memq arrangement '(stack each))
    (user-error "consult-jj: Invalid rebase arrangement `%s'" arrangement))
  (unless (cl-every #'consult-jj-commit-p sources)
    (user-error "consult-jj: Multiple rebase sources must be Commit objects"))
  (let* ((source-ids (mapcar #'consult-jj--commit-id sources))
         (seen (make-hash-table :test 'equal)))
    (dolist (id source-ids)
      (when (gethash id seen)
        (user-error "consult-jj: Duplicate rebase source"))
      (puthash id t seen))
    (let* ((identities
            (mapcar (lambda (commit)
                      (cons (consult-jj--commit-id commit)
                            (consult-jj-jj--revision-change-id
                             (consult-jj--commit-id commit) root)))
                    (consult-jj--order-rebase-sources sources root)))
           (destination-id
            (consult-jj-jj--resolve-single-revision
             (consult-jj--revision-id destination) root)))
      (make-consult-jj--rebase-plan
       :identities identities
       :destination (cons destination-id
                          (consult-jj-jj--revision-change-id destination-id root))
       :placement placement :arrangement arrangement :root root))))

(defun consult-jj--execute-rebase-plan (plan)
  (let ((root (consult-jj--rebase-plan-root plan))
        (placement (consult-jj--rebase-plan-placement plan))
        (identities (consult-jj--rebase-plan-identities plan))
        (destination (consult-jj--rebase-plan-destination plan))
        (result (make-consult-jj--rebase-result))
        previous)
    (condition-case err
        (progn
          (dolist (identity identities)
            (let* ((current (consult-jj--live-rebase-identity identity root))
                   (live-destination
                    (consult-jj--live-rebase-identity destination root))
                   (anchor
                    (if (and (eq (consult-jj--rebase-plan-arrangement plan) 'stack)
                             previous)
                        (consult-jj--live-rebase-identity previous root)
                      (if (eq placement 'before)
                          (format "parents(commit_id(%s))" live-destination)
                        live-destination))))
              (setf (consult-jj--rebase-result-attempted result) t)
              (consult-jj-jj--rebase-revisions (list current) anchor 'onto root)
              (setq previous identity)))
          (when (memq placement '(after before))
            (setf (consult-jj--rebase-result-attempted result) t)
            (consult-jj-jj--rebase-revisions
             (mapcar (lambda (identity)
                       (consult-jj--live-rebase-identity identity root))
                     identities)
             (consult-jj--live-rebase-identity destination root)
             placement root)))
      ((error quit) (setf (consult-jj--rebase-result-failure result) err)))
    result))

(defun consult-jj--live-rebase-identity (identity root)
  (consult-jj-jj--rewritten-commit-id (car identity) (cdr identity) root))

(defun consult-jj--notify-rebase-result (result operation-before root)
  (let ((inhibit-quit t))
    (condition-case err
        (let ((operation-after (consult-jj--rebase-operation-id root)))
          (setf (consult-jj--rebase-result-changed result)
                (or (null (consult-jj--rebase-result-failure result))
                    (not (equal operation-before operation-after)))))
      ((error quit)
       (setf (consult-jj--rebase-result-operation-error result) err
             (consult-jj--rebase-result-changed result)
             (consult-jj--rebase-result-attempted result))))
    (when (consult-jj--rebase-result-changed result)
      (condition-case err
          (run-hooks 'consult-jj-commit-modified-hook)
        ((error quit)
         (setf (consult-jj--rebase-result-hook-error result) err))))))

(defun consult-jj--report-rebase-result (result)
  (let ((failure (consult-jj--rebase-result-failure result))
        (operation-error (consult-jj--rebase-result-operation-error result))
        (hook-error (consult-jj--rebase-result-hook-error result)))
    (cond
     ((and failure (eq (car failure) 'quit))
      (when (consult-jj--rebase-result-changed result)
        (message "consult-jj: Rebase interrupted after partially changing the repository"))
      (signal (car failure) (cdr failure)))
     (failure
      (if (consult-jj--rebase-result-changed result)
          (error "consult-jj: Rebase partially changed the repository: %s"
                 (error-message-string failure))
        (signal (car failure) (cdr failure))))
     (operation-error
      (signal (car operation-error) (cdr operation-error)))
     (hook-error
      (signal (car hook-error) (cdr hook-error))))))

(defun consult-jj--rebase-operation-id (root)
  (string-trim
   (consult-jj-jj--run
    root "--at-operation" "@" "op" "log" "--no-graph"
    "--limit" "1" "--template" "self.id()")))

(defun consult-jj--order-rebase-sources (sources root)
  (let ((timestamps (make-hash-table :test 'eq))
        (ancestors (consult-jj--rebase-selected-ancestors sources root))
        (remaining (copy-sequence sources))
        ordered)
    (while remaining
      (let* ((eligible
              (cl-remove-if
               (lambda (candidate)
                 (cl-some
                  (lambda (other)
                    (and (not (eq candidate other))
                         (member (consult-jj--commit-id other)
                                 (gethash (consult-jj--commit-id candidate)
                                          ancestors))))
                  remaining))
               remaining))
             (next
              (progn
                (when (cdr eligible)
                  (dolist (source eligible)
                    (unless (gethash source timestamps)
                      (puthash source
                               (consult-jj--rebase-source-time source root)
                               timestamps))))
                (car (sort eligible
                           (lambda (a b)
                             (let ((a-time (gethash a timestamps))
                                   (b-time (gethash b timestamps)))
                               (if (equal a-time b-time)
                                   (string-lessp (consult-jj--commit-id a)
                                                 (consult-jj--commit-id b))
                                 (time-less-p a-time b-time)))))))))
        (unless next
          (user-error "consult-jj: Cannot order rebase sources"))
        (push next ordered)
        (setq remaining (delq next remaining))))
    (nreverse ordered)))

(defun consult-jj--rebase-source-time (source root)
  (let ((timestamp (consult-jj-commit-timestamp source)))
    (unless (and (stringp timestamp) (not (string-empty-p timestamp)))
      (setq timestamp
            (string-trim
             (consult-jj-jj--run
              root "log" "--no-graph" "--revision"
              (format "commit_id(%s)" (consult-jj--commit-id source))
              "--template" "author.timestamp().format(\"%+\")"))))
    (condition-case nil
        (date-to-time timestamp)
      (error
       (user-error "consult-jj: Invalid rebase source timestamp: %s"
                   timestamp)))))

(defun consult-jj--rebase-selected-ancestors (sources root)
  (let* ((selected
          (mapconcat (lambda (source)
                       (format "commit_id(%s)" (consult-jj--commit-id source)))
                     sources " | "))
         (ancestors (make-hash-table :test 'equal)))
    (dolist (source sources)
      (puthash
       (consult-jj--commit-id source)
       (split-string
        (consult-jj-jj--run
         root "log" "--no-graph" "--revision"
         (format "ancestors(commit_id(%s)) & (%s)"
                 (consult-jj--commit-id source) selected)
         "--template" "commit_id ++ \"\\n\"")
        "\n" t)
       ancestors))
    ancestors))

(provide 'consult-jj-rebase)
;;; consult-jj-rebase.el ends here
