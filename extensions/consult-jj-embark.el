;;; consult-jj-embark.el --- Embark actions for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (embark "1.0"))
;; Version: 0.1.0

;;; Commentary:

;; Optional Embark action maps which are enabled by `consult-jj-embark-mode'.

;;; Code:

(require 'embark)
(require 'consult-jj)

(defvar consult-jj-modified-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'consult-jj-embark-split)
    (define-key map (kbd "S") #'consult-jj-embark-squash)
    (define-key map (kbd "r") #'consult-jj-embark-restore)
    (define-key map (kbd "a") #'consult-jj-embark-absorb)
    (define-key map (kbd "d") #'consult-jj-embark-diff)
    (define-key map (kbd "e") #'consult-jj-embark-ediff)
    map)
  "Embark action map for Consult JJ modified-file candidates.")

(defvar consult-jj-modified-hunk-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'consult-jj-embark-split)
    (define-key map (kbd "S") #'consult-jj-embark-squash)
    (define-key map (kbd "r") #'consult-jj-embark-restore)
    (define-key map (kbd "a") #'consult-jj-embark-absorb)
    (define-key map (kbd "d") #'consult-jj-embark-diff)
    (define-key map (kbd "e") #'consult-jj-embark-ediff)
    map)
  "Embark action map for Consult JJ modified-hunk candidates.")

(defvar consult-jj-commit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'consult-jj-embark-commit-rebase)
    (define-key map (kbd "a") #'consult-jj-embark-commit-abandon)
    (define-key map (kbd "D") #'consult-jj-embark-commit-describe)
    (define-key map (kbd "u") #'consult-jj-embark-commit-duplicate)
    (define-key map (kbd "b") #'consult-jj-embark-commit-bookmark)
    (define-key map (kbd "n") #'consult-jj-embark-commit-new)
    (define-key map (kbd "e") #'consult-jj-embark-commit-edit)
    (define-key map (kbd "s") #'consult-jj-embark-commit-squash)
    (define-key map (kbd "d") #'consult-jj-embark-commit-diff)
    (define-key map (kbd "E") #'consult-jj-embark-commit-ediff)
    (define-key map (kbd "v") #'consult-jj-embark-commit-revert)
    (define-key map (kbd "l") #'consult-jj-embark-commit-evolution-log)
    map)
  "Embark action map for Consult JJ commit candidates.")

(defconst consult-jj-embark--keymap-entries
  '((consult-jj-modified-file consult-jj-modified-file-map embark-file-map)
    (consult-jj-modified-hunk consult-jj-modified-hunk-map embark-general-map)
    (consult-jj-commit consult-jj-commit-map embark-general-map))
  "Embark target map entries supplied by Consult JJ.")

(defconst consult-jj-embark--multitarget-actions
  '(consult-jj-embark-split
    consult-jj-embark-squash
    consult-jj-embark-restore
    consult-jj-embark-absorb
    consult-jj-embark-diff
    consult-jj-embark-ediff)
  "Consult JJ actions that over an entire Embark target set.")

(defconst consult-jj-embark--default-actions
  '((consult-jj-modified-file . find-file)
    (consult-jj-modified-hunk . consult-jj-embark-visit-hunk)
    (consult-jj-commit . consult-jj-embark-visit-commit))
  "Default actions supplied for Consult JJ target categories.")

(defvar consult-jj-embark--installed-keymap-entries nil
  "Embark target map entries installed by `consult-jj-embark-mode'.")

(defvar consult-jj-embark--installed-multitarget-actions nil
  "Multi-target actions installed by `consult-jj-embark-mode'.")

(defvar consult-jj-embark--installed-default-actions nil
  "Default actions installed by `consult-jj-embark-mode'.")

;; TODO make these work interactively as well as through embark keymaps??
(defun consult-jj-embark-split (targets)
  (consult-jj-split (consult-jj-embark--change-targets targets)))

(defun consult-jj-embark-squash (targets)
  (consult-jj-squash (consult-jj-embark--change-targets targets)))

(defun consult-jj-embark-restore (targets)
  (consult-jj-restore (consult-jj-embark--change-targets targets)))

(defun consult-jj-embark-absorb (targets)
  (consult-jj-absorb (consult-jj-embark--change-targets targets)))

(defun consult-jj-embark-diff (targets)
  "Show one combined diff for TARGETS."
  (consult-jj-diff (consult-jj-embark--change-targets targets)))

(defun consult-jj-embark-ediff (targets)
  "Ediff TARGETS."
  (consult-jj-ediff (consult-jj-embark--change-targets targets)))

(defun consult-jj-embark-commit-rebase (commit)
  "Rebase COMMIT."
  (consult-jj-commit-rebase (consult-jj-embark--commit commit)))

(defun consult-jj-embark-commit-abandon (commit)
  "Abandon COMMIT."
  (consult-jj-commit-abandon (consult-jj-embark--commit commit)))

(defun consult-jj-embark-commit-describe (commit)
  "Describe COMMIT."
  (consult-jj-commit-describe (consult-jj-embark--commit commit)))

(defun consult-jj-embark-commit-duplicate (commit)
  "Duplicate COMMIT."
  (consult-jj-commit-duplicate (consult-jj-embark--commit commit)))

(defun consult-jj-embark-commit-bookmark (commit)
  "Add a bookmark to COMMIT."
  (consult-jj-commit-bookmark (consult-jj-embark--commit commit)))

(defun consult-jj-embark-commit-new (commit)
  "Create a commit on top of COMMIT."
  (consult-jj-commit-new (consult-jj-embark--commit commit)))

(defun consult-jj-embark-commit-edit (commit)
  "Edit COMMIT."
  (consult-jj-commit-edit (consult-jj-embark--commit commit)))

(defun consult-jj-embark-commit-squash (commit)
  (consult-jj-commit-squash (consult-jj-embark--commit commit)))

(defun consult-jj-embark-commit-diff (commit)
  "Diff COMMIT."
  (consult-jj-commit-diff (consult-jj-embark--commit commit)))

(defun consult-jj-embark-commit-ediff (commit)
  "Ediff COMMIT."
  (consult-jj-commit-ediff (consult-jj-embark--commit commit)))

(defun consult-jj-embark-commit-revert (commit)
  "Revert COMMIT."
  (consult-jj-commit-revert (consult-jj-embark--commit commit)))

(defun consult-jj-embark-commit-evolution-log (commit)
  "Show evolution of COMMIT."
  (consult-jj-commit-evolution-log (consult-jj-embark--commit commit)))

(defun consult-jj-embark-visit-hunk (hunk)
  "Visit HUNK using the `consult-jj-modified-hunk' entry of `consult-jj-embark--default-actions'."
  (consult-jj-visit-hunk (consult-jj-embark--hunk hunk)))

(defun consult-jj-embark-visit-commit (commit)
  "Visit COMMIT using the `consult-jj-commit' entry of `consult-jj-embark--default-actions'."
  (funcall consult-jj-log-visit-function
           (consult-jj-commit-commit-id (consult-jj-embark--commit commit))))

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
  (dolist (entry consult-jj-embark--default-actions)
    (unless (assq (car entry) embark-default-action-overrides)
      (let ((installed (copy-tree entry)))
        (push installed embark-default-action-overrides)
        (push (copy-tree installed)
              consult-jj-embark--installed-default-actions)))))

(defun consult-jj-embark--disable ()
  "Remove Consult JJ target map registrations owned by the mode."
  (dolist (entry consult-jj-embark--installed-keymap-entries)
    (setq embark-keymap-alist (delete entry embark-keymap-alist)))
  (dolist (action consult-jj-embark--installed-multitarget-actions)
    (setq embark-multitarget-actions
          (delq action embark-multitarget-actions)))
  (dolist (entry consult-jj-embark--installed-default-actions)
    (setq embark-default-action-overrides
          (delete entry embark-default-action-overrides)))
  (setq consult-jj-embark--installed-keymap-entries nil
        consult-jj-embark--installed-multitarget-actions nil
        consult-jj-embark--installed-default-actions nil))

;; TODO why is any of the below needed?
(defun consult-jj-embark--change-targets (targets)
  "Return file names or hunk objects carried by Embark TARGETS."
  (mapcar (lambda (target)
            (or (get-text-property 0 'consult-jj-hunk target)
                (substring-no-properties target)))
          targets))

(defun consult-jj-embark--commit (target)
  "Return the structured commit carried by Embark TARGET."
  (or (get-text-property 0 'consult-jj-commit target)
      (user-error "consult-jj: Embark target does not carry a commit")))

(defun consult-jj-embark--hunk (target)
  "Return the structured hunk carried by Embark TARGET."
  (or (get-text-property 0 'consult-jj-hunk target)
      (user-error "consult-jj: Embark target does not carry a hunk")))

(provide 'consult-jj-embark)
;;; consult-jj-embark.el ends here
