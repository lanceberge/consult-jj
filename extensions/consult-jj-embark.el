;;; consult-jj-embark.el --- Embark actions for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (embark "1.0"))
;; Version: 0.1.0

;;; Commentary:

;; Optional Embark action maps which are enabled by `consult-jj-embark-mode'.

;;; Code:

(require 'cl-lib)
(require 'embark)
(require 'consult-jj)

(defvar consult-jj-modified-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'consult-jj-split)
    (define-key map (kbd "S") #'consult-jj-squash)
    (define-key map (kbd "r") #'consult-jj-restore)
    (define-key map (kbd "a") #'consult-jj-absorb)
    (define-key map (kbd "d") #'consult-jj-diff)
    (define-key map (kbd "e") #'consult-jj-ediff)
    map)
  "Embark action map for Consult JJ modified-file candidates.")

(defvar consult-jj-modified-hunk-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'consult-jj-split)
    (define-key map (kbd "S") #'consult-jj-squash)
    (define-key map (kbd "r") #'consult-jj-restore)
    (define-key map (kbd "a") #'consult-jj-absorb)
    (define-key map (kbd "d") #'consult-jj-diff)
    (define-key map (kbd "e") #'consult-jj-ediff)
    map)
  "Embark action map for Consult JJ modified-hunk candidates.")

(defvar consult-jj-commit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'consult-jj-modified-files-in-commit)
    (define-key map (kbd "h") #'consult-jj-modified-hunks-in-commit)
    (define-key map (kbd "r") #'consult-jj-rebase)
    (define-key map (kbd "a") #'consult-jj-commit-abandon)
    (define-key map (kbd "D") #'consult-jj-commit-describe)
    (define-key map (kbd "u") #'consult-jj-commit-duplicate)
    (define-key map (kbd "b") #'consult-jj-bookmark-set)
    (define-key map (kbd "n") #'consult-jj-new-here)
    (define-key map (kbd "N") #'consult-jj-new)
    (define-key map (kbd "e") #'consult-jj-commit-edit)
    (define-key map (kbd "s") #'consult-jj-commit-squash)
    (define-key map (kbd "d") #'consult-jj-commit-diff)
    (define-key map (kbd "E") #'consult-jj-commit-ediff)
    (define-key map (kbd "v") #'consult-jj-commit-revert)
    (define-key map (kbd "l") #'consult-jj-commit-evolution-log)
    map)
  "Embark action map for Consult JJ commit candidates.")

(defvar consult-jj-bookmark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'consult-jj-bookmark-advance)
    (define-key map (kbd "b") #'consult-jj-bookmark-set)
    (define-key map (kbd "m") #'consult-jj-bookmark-move)
    (define-key map (kbd "n") #'consult-jj-new-here)
    map)
  "Embark action map for Consult JJ bookmark candidates.")

(defvar consult-jj-operation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'consult-jj-op-show)
    (define-key map (kbd "d") #'consult-jj-op-diff)
    (define-key map (kbd "r") #'consult-jj-op-revert)
    (define-key map (kbd "R") #'consult-jj-op-restore)
    map)
  "Embark action map for Consult JJ operation-log candidates.")

(defvar consult-jj-workspace-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'consult-jj-workspace-select)
    map)
  "Embark action map for Consult JJ workspace candidates.")

(defvar consult-jj-embark-become-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'consult-jj-modified-files)
    (define-key map (kbd "h") #'consult-jj-modified-hunks)
    (define-key map (kbd "l") #'consult-jj-log)
    (define-key map (kbd "b") #'consult-jj-bookmark)
    (define-key map (kbd "o") #'consult-jj-op-log)
    (define-key map (kbd "w") #'consult-jj-workspace-list)
    map)
  "Embark become map for Consult JJ discovery commands.")

(defvar consult-jj-embark-log-become-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'consult-jj-log)
    (define-key map (kbd "+") #'consult-jj-log-expand)
    (define-key map (kbd "-") #'consult-jj-log-shrink)
    map)
  "Embark Become map for commit-log tier transitions.")

(defvar consult-jj-embark-op-log-become-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'consult-jj-op-log)
    (define-key map (kbd "+") #'consult-jj-op-log-expand)
    (define-key map (kbd "-") #'consult-jj-op-log-shrink)
    map)
  "Embark Become map for operation-log tier transitions.")

(defconst consult-jj-embark--keymap-entries
  '((consult-jj-modified-file consult-jj-modified-file-map embark-file-map)
    (consult-jj-modified-hunk consult-jj-modified-hunk-map embark-general-map)
    (consult-jj-commit consult-jj-commit-map embark-general-map)
    (consult-jj-bookmark consult-jj-bookmark-map embark-general-map)
    (consult-jj-operation consult-jj-operation-map embark-general-map)
    (consult-jj-workspace consult-jj-workspace-map embark-general-map))
  "Embark target map entries supplied by Consult JJ.")

(defconst consult-jj-embark--multitarget-actions
  '(consult-jj-split
    consult-jj-squash
    consult-jj-restore
    consult-jj-absorb
    consult-jj-diff
    consult-jj-ediff
    consult-jj-modified-files-in-commit
    consult-jj-modified-hunks-in-commit
    consult-jj-commit-squash
    consult-jj-bookmark-move
    consult-jj-bookmark-advance
    consult-jj-op-diff)
  "Consult JJ actions Embark invokes non-interactively with adapted targets.")

(defconst consult-jj-embark--default-actions
  '((consult-jj-modified-file . find-file)
    (consult-jj-modified-hunk . consult-jj-visit-hunk)
    (consult-jj-commit . consult-jj-default-log-visit)
    (consult-jj-bookmark . consult-jj-new-here)
    (consult-jj-operation . consult-jj-op-show)
    (consult-jj-workspace . consult-jj-workspace-select))
  "Default actions supplied for Consult JJ target categories.")

(defconst consult-jj-embark--around-action-hooks
  '((consult-jj-split . consult-jj-embark--change-targets)
    (consult-jj-squash . consult-jj-embark--change-targets)
    (consult-jj-restore . consult-jj-embark--change-targets)
    (consult-jj-absorb . consult-jj-embark--change-targets)
    (consult-jj-diff . consult-jj-embark--change-targets)
    (consult-jj-ediff . consult-jj-embark--change-targets)
    (consult-jj-visit-hunk . consult-jj-embark--hunk-target)
    (consult-jj-default-log-visit . consult-jj-embark--commit-id-target)
    (consult-jj-modified-files-in-commit
     . consult-jj-embark--single-commit-target)
    (consult-jj-modified-hunks-in-commit
     . consult-jj-embark--single-commit-target)
    (consult-jj-rebase . consult-jj-embark--commit-target)
    (consult-jj-new-here . consult-jj-embark--revision-target)
    (consult-jj-new . consult-jj-embark--commit-target)
    (consult-jj-commit-abandon . consult-jj-embark--commit-target)
    (consult-jj-commit-describe . consult-jj-embark--commit-target)
    (consult-jj-commit-duplicate . consult-jj-embark--commit-target)
    (consult-jj-bookmark-move . consult-jj-embark--bookmark-move-source)
    (consult-jj-bookmark-advance . consult-jj-embark--bookmark-targets)
    (consult-jj-bookmark-set . consult-jj-embark--bookmark-set-target)
    (consult-jj-commit-edit . consult-jj-embark--commit-target)
    (consult-jj-commit-squash . consult-jj-embark--commit-targets)
    (consult-jj-commit-diff . consult-jj-embark--commit-target)
    (consult-jj-commit-ediff . consult-jj-embark--commit-target)
    (consult-jj-commit-revert . consult-jj-embark--commit-target)
    (consult-jj-commit-evolution-log . consult-jj-embark--commit-target)
    (consult-jj-op-diff . consult-jj-embark--operation-diff-targets)
    (consult-jj-op-revert . consult-jj-embark--operation-target)
    (consult-jj-op-restore . consult-jj-embark--operation-target)
    (consult-jj-op-show . consult-jj-embark--operation-target)
    (consult-jj-workspace-select . consult-jj-embark--workspace-target))
  "Around hooks that adapt Consult JJ candidates for core actions.")

(defvar consult-jj-embark--installed-keymap-entries nil
  "Embark target map entries installed by `consult-jj-embark-mode'.")

(defvar consult-jj-embark--installed-multitarget-actions nil
  "Multi-target actions installed by `consult-jj-embark-mode'.")

(defvar consult-jj-embark--installed-default-actions nil
  "Default actions installed by `consult-jj-embark-mode'.")

(defvar consult-jj-embark--installed-around-action-hooks nil
  "Actions whose around hook was installed by `consult-jj-embark-mode'.")

(defvar consult-jj-embark--installed-around-action-entries nil
  "Around-hook alist entries created by `consult-jj-embark-mode'.")

(defvar consult-jj-embark--installed-refresh-hook nil
  "Non-nil when the mode installed its live-refresh cleanup hook.")

;; TODO make these work interactively as well as through embark keymaps??
(defvar consult-jj-embark--installed-become-keymaps nil
  "Become keymaps installed by `consult-jj-embark-mode'.")

;;;###autoload
(define-minor-mode consult-jj-embark-mode
  "Toggle global Embark integration for Consult JJ candidates."
  :global t
  :group 'consult-jj
  (if consult-jj-embark-mode
      (consult-jj-embark--enable)
    (consult-jj-embark--disable)))

(defun consult-jj-embark--enable ()
  "Register Consult JJ target maps with Embark."
  (dolist (entry consult-jj-embark--keymap-entries)
    (unless (assq (car entry) embark-keymap-alist)
      (let ((installed (copy-tree entry)))
        (push installed embark-keymap-alist)
        (push (copy-tree installed)
              consult-jj-embark--installed-keymap-entries))))
  (dolist (action consult-jj-embark--multitarget-actions)
    (unless (memq action embark-multitarget-actions)
      (push action embark-multitarget-actions)
      (push action consult-jj-embark--installed-multitarget-actions)))
  (dolist (registration consult-jj-embark--around-action-hooks)
    (let* ((action (car registration))
           (hook (cdr registration))
           (entry (assq action embark-around-action-hooks)))
      (unless (memq hook (cdr entry))
        (unless entry
          (setq entry (list action))
          (push entry embark-around-action-hooks)
          (push entry consult-jj-embark--installed-around-action-entries))
        (push hook (cdr entry))
        (push action consult-jj-embark--installed-around-action-hooks))))
  (dolist (entry consult-jj-embark--default-actions)
    (unless (assq (car entry) embark-default-action-overrides)
      (let ((installed (copy-tree entry)))
        (push installed embark-default-action-overrides)
        (push (copy-tree installed)
              consult-jj-embark--installed-default-actions))))
  (unless (memq 'consult-jj-embark-become-map embark-become-keymaps)
    (push 'consult-jj-embark-become-map embark-become-keymaps)
    (push 'consult-jj-embark-become-map
          consult-jj-embark--installed-become-keymaps))
  (unless (memq 'consult-jj-embark-log-become-map embark-become-keymaps)
    (push 'consult-jj-embark-log-become-map embark-become-keymaps)
    (push 'consult-jj-embark-log-become-map
          consult-jj-embark--installed-become-keymaps))
  (unless (memq 'consult-jj-embark-op-log-become-map embark-become-keymaps)
    (push 'consult-jj-embark-op-log-become-map embark-become-keymaps)
    (push 'consult-jj-embark-op-log-become-map
          consult-jj-embark--installed-become-keymaps))
  (unless (memq #'consult-jj-embark--clear-selection
                consult-jj-candidate-session-refreshed-hook)
    (add-hook 'consult-jj-candidate-session-refreshed-hook
              #'consult-jj-embark--clear-selection)
    (setq consult-jj-embark--installed-refresh-hook t)))

(defun consult-jj-embark--disable ()
  "Remove Consult JJ target map registrations owned by the mode."
  (dolist (entry consult-jj-embark--installed-keymap-entries)
    (setq embark-keymap-alist (delete entry embark-keymap-alist)))
  (dolist (action consult-jj-embark--installed-multitarget-actions)
    (setq embark-multitarget-actions
          (delq action embark-multitarget-actions)))
  (dolist (action consult-jj-embark--installed-around-action-hooks)
    (when-let* ((entry (assq action embark-around-action-hooks)))
      (setcdr entry
              (delq (alist-get action consult-jj-embark--around-action-hooks)
                    (cdr entry)))
      (when (and (memq entry consult-jj-embark--installed-around-action-entries)
                 (null (cdr entry)))
        (setq embark-around-action-hooks
              (delq entry embark-around-action-hooks)))))
  (dolist (entry consult-jj-embark--installed-default-actions)
    (setq embark-default-action-overrides
          (delete entry embark-default-action-overrides)))
  (dolist (map consult-jj-embark--installed-become-keymaps)
    (setq embark-become-keymaps (delq map embark-become-keymaps)))
  (when consult-jj-embark--installed-refresh-hook
    (remove-hook 'consult-jj-candidate-session-refreshed-hook
                 #'consult-jj-embark--clear-selection))
  (setq consult-jj-embark--installed-keymap-entries nil
        consult-jj-embark--installed-multitarget-actions nil
        consult-jj-embark--installed-default-actions nil
        consult-jj-embark--installed-around-action-hooks nil
        consult-jj-embark--installed-around-action-entries nil
        consult-jj-embark--installed-refresh-hook nil
        consult-jj-embark--installed-become-keymaps nil))

(defun consult-jj-embark--clear-selection ()
  "Clear Embark selections owned by the refreshed minibuffer."
  (dolist (selection embark--selection)
    (when (overlayp (cdr selection))
      (delete-overlay (cdr selection))))
  (setq embark--selection nil)
  (force-mode-line-update t))

(cl-defun consult-jj-embark--change-targets
    (&rest args &key run action candidates target &allow-other-keys)
  "Run an Embark action after adapting its annotated change targets.
RUN receives ARGS with CANDIDATES replaced by structured targets.  The wrapped
ACTION receives their first source revision once through `:source-rev'.  No
repository root crosses the Embark seam."
  (let* ((targets
          (mapcar #'consult-jj-embark--change-target
                  (or candidates (list target))))
         (source-rev (consult-jj-embark--change-source-rev (car targets)))
         (arguments (copy-sequence args)))
    (setq arguments (plist-put arguments :candidates targets))
    (setq arguments
          (plist-put
           arguments :action
           (lambda (adapted-targets)
             (funcall action adapted-targets :source-rev source-rev))))
    (apply run arguments)))

(defun consult-jj-embark--change-target (candidate)
  "Return the structured modified target carried by CANDIDATE."
  (or (get-text-property 0 'consult-jj-hunk candidate)
      (get-text-property 0 'consult-jj-modified-file candidate)
      (user-error
       "consult-jj: Embark candidate does not carry a structured modified target")))

(defun consult-jj-embark--change-source-rev (target)
  "Return TARGET's normalized source revision."
  (if (consult-jj-hunk-p target)
      (consult-jj-hunk-source-rev target)
    (consult-jj-modified-file-source-rev target)))

(cl-defun consult-jj-embark--hunk-target
    (&rest args &key run target &allow-other-keys)
  "Run an Embark action with the hunk carried by TARGET."
  (apply run
         (plist-put
          (copy-sequence args)
          :target
          (or (get-text-property 0 'consult-jj-hunk target)
              (user-error "consult-jj: Embark target does not carry a hunk")))))

(cl-defun consult-jj-embark--commit-id-target
    (&rest args &key run target &allow-other-keys)
  "Run an Embark action with the commit ID carried by TARGET."
  (let ((commit
         (or (get-text-property 0 'consult-jj-commit target)
             (user-error
              "consult-jj: Embark target does not carry a commit"))))
    (apply run
           (plist-put
            (copy-sequence args)
            :target
            (consult-jj-commit-commit-id commit)))))

(cl-defun consult-jj-embark--revision-target
    (&rest args &key run target &allow-other-keys)
  "Pass ARGS to RUN with TARGET replaced by its carried revision."
  (let ((bookmark (get-text-property 0 'consult-jj-bookmark target))
        (commit (get-text-property 0 'consult-jj-commit target)))
    (apply run
           (plist-put
            (copy-sequence args)
            :target
            (cond
             (bookmark (consult-jj-bookmark-revision bookmark))
             (commit commit)
             (t
              (user-error
               "consult-jj: Embark target does not carry a revision")))))))

(cl-defun consult-jj-embark--commit-target
    (&rest args &key run target &allow-other-keys)
  "Run an Embark action with the commit object carried by TARGET."
  (apply run
         (plist-put
          (copy-sequence args)
          :target
          (or (get-text-property 0 'consult-jj-commit target)
              (user-error
               "consult-jj: Embark target does not carry a commit")))))

(cl-defun consult-jj-embark--single-commit-target
    (&rest args &key run action candidates target &allow-other-keys)
  "Run one change-browsing ACTION with one structured source commit."
  (let ((candidates (or candidates (list target))))
    (unless (= (length candidates) 1)
      (user-error
       "consult-jj: Change browsing accepts exactly one source commit"))
    (let ((commit
           (or (get-text-property 0 'consult-jj-commit (car candidates))
               (user-error
                "consult-jj: Embark target does not carry a commit"))))
      (apply run
             (plist-put
              (copy-sequence args)
              :action
              (lambda (_targets)
                (funcall action commit)))))))

(cl-defun consult-jj-embark--commit-targets
    (&rest args &key run candidates target &allow-other-keys)
  "Run an Embark action with the commit objects carried by its target set."
  (apply run
         (plist-put
          (copy-sequence args)
          :candidates
          (mapcar
           (lambda (candidate)
             (or (get-text-property 0 'consult-jj-commit candidate)
                 (user-error
                  "consult-jj: Embark target does not carry a commit")))
           (or candidates (list target))))))

(cl-defun consult-jj-embark--operation-target
    (&rest args &key run target &allow-other-keys)
  "Run an Embark action with the operation object carried by TARGET."
  (apply run
         (plist-put
          (copy-sequence args)
          :target
          (or (get-text-property 0 'consult-jj-operation target)
              (user-error
               "consult-jj: Embark target does not carry an operation")))))

(cl-defun consult-jj-embark--workspace-target
    (&rest args &key run target &allow-other-keys)
  "Run an Embark action with the workspace object carried by TARGET."
  (apply run
         (plist-put
          (copy-sequence args)
          :target
          (or (get-text-property 0 'consult-jj-workspace target)
              (user-error
               "consult-jj: Embark target does not carry a workspace")))))

(cl-defun consult-jj-embark--operation-diff-targets
    (&rest args &key run action candidates target &allow-other-keys)
  "Run operation diff with annotated targets ordered from lower to upper.
RUN receives ARGS with ACTION adapted to pass the structured operations as
separate positional arguments."
  (let* ((candidates (or candidates (list target)))
         (count (length candidates)))
    (when (> count 2)
      (user-error "consult-jj: Operation diff accepts at most two targets"))
    (let ((operations
           (mapcar
            (lambda (candidate)
              (or (get-text-property 0 'consult-jj-operation candidate)
                  (user-error
                   "consult-jj: Embark target does not carry an operation")))
            (sort
             (copy-sequence candidates)
             (lambda (left right)
               (> (or (get-text-property 0 'consult-jj-op-log-index left) 0)
                  (or (get-text-property 0 'consult-jj-op-log-index right)
                      0)))))))
      (apply run
             (plist-put
              (copy-sequence args)
              :action
              (lambda (_targets)
                (apply action operations)))))))

(cl-defun consult-jj-embark--bookmark-move-source
    (&rest args &key run target &allow-other-keys)
  "Run bookmark move with the structured bookmark carried by TARGET."
  (apply run
         (plist-put
          (copy-sequence args)
          :candidates
          (or (get-text-property 0 'consult-jj-bookmark target)
              (user-error
               "consult-jj: Embark target does not carry a bookmark")))))

(cl-defun consult-jj-embark--bookmark-targets
    (&rest args &key run candidates target &allow-other-keys)
  "Run bookmark advancement with structured bookmark CANDIDATES."
  (apply run
         (plist-put
          (copy-sequence args)
          :candidates
          (mapcar
           (lambda (candidate)
             (or (get-text-property 0 'consult-jj-bookmark candidate)
                 (user-error
                  "consult-jj: Embark target does not carry a bookmark")))
           (or candidates (list target))))))

(cl-defun consult-jj-embark--bookmark-set-target
    (&rest args &key run target &allow-other-keys)
  "Run bookmark set with the structured object carried by TARGET."
  (apply run
         (plist-put
          (copy-sequence args)
          :target
          (or (get-text-property 0 'consult-jj-bookmark target)
              (get-text-property 0 'consult-jj-commit target)
              (user-error
               "consult-jj: Embark target has no bookmark destination")))))

(provide 'consult-jj-embark)
;;; consult-jj-embark.el ends here
