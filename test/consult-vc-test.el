;;; consult-vc-test.el --- Tests for the consult-vc hunk UI -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for hunk candidate identity and preview availability.

;;; Code:

(require 'ert)
(require 'consult-vc)

(defun consult-vc-test--hunk (path &optional status)
  "Return a test hunk for PATH with STATUS."
  (consult-vc-hunk-create
   :provider 'jj :root "/repo/" :source-rev "@"
   :old-path path :new-path (unless (eq status 'deleted) path)
   :status (or status 'modified) :new-start 1 :supported t
   :hunk-header "@@ -1 +1 @@ context"))

(ert-deftest consult-vc-modified-hunks-visits-selected-hunk ()
  (let* ((root (make-temp-file "consult-vc-test-" t))
         (path (expand-file-name "file.txt" root))
         (hunk (consult-vc-test--hunk "file.txt"))
         (provider (consult-vc-provider-def-create
                    :name 'test
                    :collect-files #'ignore
                    :collect-hunks (lambda (_root) (list hunk)))))
    (setf (consult-vc-hunk-new-start hunk) 2)
    (unwind-protect
        (progn
          (write-region "first\nselected\nthird\n" nil path nil 'silent)
          (with-temp-buffer
            (cl-letf (((symbol-function 'consult-vc--root) (lambda () root))
                      ((symbol-function 'consult-vc-provider-resolve)
                       (lambda (_root) provider))
                      ((symbol-function 'consult--read)
                       (lambda (&rest _) hunk)))
              (consult-vc-modified-hunks)
              (should (equal buffer-file-name path))
              (should (= (line-number-at-pos) 2)))))
      (when-let ((buffer (get-file-buffer path))) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest consult-vc-hunk-state-does-not-jump-to-returned-hunk-object ()
  "Consult's return event must not treat a looked-up hunk as a marker."
  (let* ((hunk (consult-vc-test--hunk "file.txt"))
         (candidate (propertize "candidate" 'consult-vc-hunk hunk))
         (state (consult-vc--hunk-state (list candidate))))
    (funcall state 'return hunk)))

(ert-deftest consult-vc-hunk-state-previews-looked-up-hunk-object ()
  (let* ((root (make-temp-file "consult-vc-test-" t))
         (path (expand-file-name "file.txt" root))
         (hunk (consult-vc-test--hunk "file.txt")))
    (setf (consult-vc-hunk-new-start hunk) 2)
    (unwind-protect
        (progn
          (write-region "first\npreviewed\nthird\n" nil path nil 'silent)
          (let* ((candidate (consult-vc--hunk-candidate hunk root))
                 (state (consult-vc--hunk-state (list candidate))))
            (funcall state 'preview hunk)
            (should (equal buffer-file-name path))
            (should (= (line-number-at-pos) 2))
            (funcall state 'preview nil)))
      (when-let ((buffer (get-file-buffer path))) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest consult-vc-hunk-candidate-retains-object-and-location ()
  (let* ((root (make-temp-file "consult-vc-test-" t))
         (path (expand-file-name "file.txt" root))
         (hunk (consult-vc-test--hunk "file.txt")))
    (unwind-protect
        (progn
          (write-region "changed\n" nil path nil 'silent)
          (let ((candidate (consult-vc--hunk-candidate hunk root)))
            (should (eq (get-text-property 0 'consult-vc-hunk candidate) hunk))
            (should (get-text-property 0 'consult-location candidate))
            (should (eq (consult-vc--lookup-hunk candidate (list candidate)) hunk))))
      (when-let ((buffer (get-file-buffer path))) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest consult-vc-hunk-candidate-keeps-nonpreviewable-hunk ()
  (let* ((root (make-temp-file "consult-vc-test-" t))
         (hunk (consult-vc-test--hunk "deleted.txt" 'deleted))
         (candidate (consult-vc--hunk-candidate hunk root)))
    (unwind-protect
        (progn
          (should (eq (get-text-property 0 'consult-vc-hunk candidate) hunk))
          (should-not (get-text-property 0 'consult-location candidate))
          (should (string-match-p "preview unavailable" candidate)))
      (delete-directory root t))))

(provide 'consult-vc-test)
;;; consult-vc-test.el ends here
