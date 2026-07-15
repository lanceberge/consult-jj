;;; consult-vc-provider-test.el --- Tests for the provider registry -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `consult-vc-provider': the required-protocol validation,
;; registry lookup, and provider resolution (explicit and `discover').
;; These do not load Consult, Embark, or any backend.

;;; Code:

(require 'ert)
(require 'consult-vc-provider)

(defmacro consult-vc-provider-test--with-clean-registry (&rest body)
  "Run BODY with an empty, isolated provider registry and default option."
  (declare (indent 0) (debug t))
  `(let ((consult-vc-provider--registry (make-hash-table :test 'eq))
         (consult-vc-provider 'discover))
     ,@body))

(defun consult-vc-provider-test--def (name)
  "Return a minimal valid provider def named NAME."
  (consult-vc-provider-def-create
   :name name
   :collect-files (lambda (_root) nil)
   :collect-hunks (lambda (_root) nil)))

(ert-deftest consult-vc-provider-default-is-discover ()
  "The provider selector defaults to `discover'."
  (should (eq (default-value 'consult-vc-provider) 'discover)))

(ert-deftest consult-vc-provider-register-and-get ()
  "A registered provider can be looked up by its name symbol."
  (consult-vc-provider-test--with-clean-registry
    (let ((def (consult-vc-provider-test--def 'jj)))
      (should (eq (consult-vc-provider-register def) def))
      (should (eq (consult-vc-provider-get 'jj) def))
      (should (null (consult-vc-provider-get 'git))))))

(ert-deftest consult-vc-provider-register-replaces-same-name ()
  "Registering a provider replaces an earlier one with the same name."
  (consult-vc-provider-test--with-clean-registry
    (let ((old (consult-vc-provider-test--def 'jj))
          (new (consult-vc-provider-test--def 'jj)))
      (consult-vc-provider-register old)
      (consult-vc-provider-register new)
      (should (eq (consult-vc-provider-get 'jj) new))
      (should (equal (consult-vc-provider-names) '(jj))))))

(ert-deftest consult-vc-provider-names-lists-registered ()
  "`consult-vc-provider-names' returns every registered provider symbol."
  (consult-vc-provider-test--with-clean-registry
    (consult-vc-provider-register (consult-vc-provider-test--def 'jj))
    (consult-vc-provider-register (consult-vc-provider-test--def 'git))
    (should (equal (sort (consult-vc-provider-names) #'string<)
                   '(git jj)))))

(ert-deftest consult-vc-provider-register-requires-protocol ()
  "Registration rejects providers missing a required protocol function."
  (consult-vc-provider-test--with-clean-registry
    (should-error (consult-vc-provider-register 'jj))
    (should-error
     (consult-vc-provider-register
      (consult-vc-provider-def-create :name 'jj
                                      :collect-files (lambda (_r) nil))))
    (should-error
     (consult-vc-provider-register
      (consult-vc-provider-def-create :name 'jj
                                      :collect-hunks (lambda (_r) nil))))))

(ert-deftest consult-vc-provider-resolve-explicit ()
  "An explicit provider symbol resolves to that provider, ignoring the VC backend."
  (consult-vc-provider-test--with-clean-registry
    (let ((def (consult-vc-provider-test--def 'jj))
          (consult-vc-provider 'jj))
      (consult-vc-provider-register def)
      (should (eq (consult-vc-provider-resolve "/somewhere") def)))))

(ert-deftest consult-vc-provider-resolve-discover ()
  "`discover' maps the VC backend symbol to a downcased provider name."
  (consult-vc-provider-test--with-clean-registry
    (let ((def (consult-vc-provider-test--def 'jj)))
      (consult-vc-provider-register def)
      (cl-letf (((symbol-function 'vc-responsible-backend) (lambda (_root) 'JJ)))
        (should (eq (consult-vc-provider-resolve "/repo") def))))))

(ert-deftest consult-vc-provider-resolve-errors-when-missing ()
  "Resolution errors when no provider matches the discovered backend."
  (consult-vc-provider-test--with-clean-registry
    (cl-letf (((symbol-function 'vc-responsible-backend) (lambda (_root) 'Git)))
      (should-error (consult-vc-provider-resolve "/repo") :type 'user-error))))

(ert-deftest consult-vc-provider-resolve-errors-without-backend ()
  "Resolution errors when the root is not under version control."
  (consult-vc-provider-test--with-clean-registry
    (cl-letf (((symbol-function 'vc-responsible-backend) (lambda (_root) nil)))
      (should-error (consult-vc-provider-resolve "/tmp") :type 'user-error))))

(ert-deftest consult-vc-action-fields ()
  "`consult-vc-action' carries label, key, and fn."
  (let ((action (consult-vc-action-create
                 :label "Export diff" :key "d" :fn #'ignore)))
    (should (equal (consult-vc-action-label action) "Export diff"))
    (should (equal (consult-vc-action-key action) "d"))
    (should (eq (consult-vc-action-fn action) #'ignore))))

(provide 'consult-vc-provider-test)
;;; consult-vc-provider-test.el ends here
