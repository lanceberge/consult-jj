;;; consult-jj-diff-hl.el --- Diff-HL integration for consult-jj -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (diff-hl "1.10.0"))
;; Version: 0.1.0

;;; Commentary:

;; Add `consult-jj-diff-hl-mode' which refreshes diff-hl buffers after consult-jj modifies repository contents.

;;; Code:

(require 'cl-lib)
(require 'diff-hl)
(require 'consult-jj)

(defvar consult-jj-diff-hl--installed-hook nil
  "Non-nil when the mode installed its repository-modified hook.")

(defvar consult-jj-diff-hl--installed-advice nil)

;;;###autoload
(define-minor-mode consult-jj-diff-hl-mode
  "Toggle global Diff-HL integration for consult-jj mutations."
  :global t
  :group 'consult-jj
  (if consult-jj-diff-hl-mode
      (consult-jj-diff-hl--enable)
    (consult-jj-diff-hl--disable)))

(defun consult-jj-diff-hl--enable ()
  "Refresh Diff-HL buffers after consult-jj mutations."
  (unless (memq #'consult-jj-diff-hl--refresh
                consult-jj-commit-modified-hook)
    (add-hook 'consult-jj-commit-modified-hook
              #'consult-jj-diff-hl--refresh)
    (setq consult-jj-diff-hl--installed-hook t))
  (unless (advice-member-p #'consult-jj-diff-hl--diff-against-reference
                           'diff-hl-diff-against-reference)
    (advice-add 'diff-hl-diff-against-reference :around
                #'consult-jj-diff-hl--diff-against-reference)
    (setq consult-jj-diff-hl--installed-advice t)))

(defun consult-jj-diff-hl--disable ()
  "Remove the mutation hook installed by the mode."
  (when consult-jj-diff-hl--installed-hook
    (remove-hook 'consult-jj-commit-modified-hook
                 #'consult-jj-diff-hl--refresh))
  (setq consult-jj-diff-hl--installed-hook nil)
  (when consult-jj-diff-hl--installed-advice
    (advice-remove 'diff-hl-diff-against-reference
                   #'consult-jj-diff-hl--diff-against-reference))
  (setq consult-jj-diff-hl--installed-advice nil))

(defun consult-jj-diff-hl--diff-against-reference
    (original file backend buffer &optional new-rev)
  (if (and (eq backend 'JJ)
           (null diff-hl-reference-revision)
           (null new-rev))
      (progn
        (require 'vc-jj)
        (let ((previous-revision (symbol-function 'vc-jj-previous-revision)))
          (cl-letf (((symbol-function 'vc-jj-previous-revision)
                     (lambda (target revision)
                       (if (and (null target) (equal revision "@"))
                           (string-trim
                            (consult-jj-jj--run
                             (vc-jj-root file)
                             "--ignore-working-copy" "log" "--no-graph"
                             "-r" "first_parent(@)" "-T" "commit_id"))
                         (funcall previous-revision target revision)))))
            (funcall original file backend buffer new-rev))))
    (funcall original file backend buffer new-rev)))

(defun consult-jj-diff-hl--refresh ()
  "Refresh Diff-HL buffers in the repository just modified by consult-jj."
  (when consult-jj--commit-modified-root
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (let ((file buffer-file-name))
          (when (and diff-hl-mode
                     file
                     (not (buffer-modified-p))
                     (file-exists-p file)
                     (file-in-directory-p
                      file consult-jj--commit-modified-root))
            (when-let* ((backend (vc-backend file)))
              (vc-state-refresh file backend)
              (diff-hl-update))))))))

(provide 'consult-jj-diff-hl)
;;; consult-jj-diff-hl.el ends here
