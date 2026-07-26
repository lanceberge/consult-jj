;;; consult-jj-diff-hl.el --- Diff-HL integration for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (diff-hl "1.10.0"))
;; Version: 0.1.0

;;; Commentary:

;; Add `consult-jj-diff-hl-mode' which refreshes diff-hl buffers after consult-jj modifies repository contents.

;;; Code:

(require 'diff-hl)
(require 'consult-jj)

(defvar consult-jj-diff-hl--installed-hook nil
  "Non-nil when the mode installed its repository-modified hook.")

;;;###autoload
(define-minor-mode consult-jj-diff-hl-mode
  "Toggle global Diff-HL integration for Consult JJ mutations."
  :global t
  :group 'consult-jj
  (if consult-jj-diff-hl-mode
      (consult-jj-diff-hl--enable)
    (consult-jj-diff-hl--disable)))

(defun consult-jj-diff-hl--enable ()
  "Refresh Diff-HL buffers after Consult JJ mutations."
  (unless (memq #'consult-jj-diff-hl--refresh
                consult-jj-commit-modified-hook)
    (add-hook 'consult-jj-commit-modified-hook
              #'consult-jj-diff-hl--refresh)
    (setq consult-jj-diff-hl--installed-hook t)))

(defun consult-jj-diff-hl--disable ()
  "Remove the mutation hook installed by the mode."
  (when consult-jj-diff-hl--installed-hook
    (remove-hook 'consult-jj-commit-modified-hook
                 #'consult-jj-diff-hl--refresh))
  (setq consult-jj-diff-hl--installed-hook nil))

(defun consult-jj-diff-hl--refresh ()
  "Refresh Diff-HL buffers in the repository just modified by Consult JJ."
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
            (when-let ((backend (vc-backend file)))
              (vc-state-refresh file backend)
              (diff-hl-update))))))))

(provide 'consult-jj-diff-hl)
;;; consult-jj-diff-hl.el ends here
