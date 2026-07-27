;;; consult-jj-session.el --- Live candidate sessions for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (consult "1.0"))

;;; Commentary:

;; Internal machinery for replacing candidates in active Consult JJ sessions.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'consult)

(cl-defstruct (consult-jj--candidate-session
               (:constructor consult-jj--candidate-session-create)
               (:copier nil))
  "One active Consult JJ candidate session."
  root view tier buffer replace collect present)

(defvar consult-jj--candidate-sessions nil
  "Currently active Consult JJ candidate sessions.")

(defvar consult-jj--candidate-session-adapters nil
  "Refresh adapters keyed by candidate-session view.")

(defvar consult-jj--candidate-refresh-context nil
  "Shared mutable state for coalescing one semantic mutation's refresh.")

(defun consult-jj--register-candidate-session-adapter
    (view collect present)
  "Register COLLECT and PRESENT refresh functions for VIEW."
  (setf (alist-get view consult-jj--candidate-session-adapters)
        (cons collect present)))

(defun consult-jj--root ()
  "Return the current project root, or signal a `user-error'."
  (let ((project (project-current nil)))
    (unless project
      (user-error "consult-jj: No project found for %s" default-directory))
    (expand-file-name (project-root project))))

(defun consult-jj--live-candidate-collection
    (initial root view &optional tier collect present)
  "Return a live Consult collection for INITIAL candidates under ROOT.
VIEW identifies the candidate presentation to refresh.  TIER, when
non-nil, identifies the retained discovery tier.  COLLECT receives
ROOT and TIER to refresh source objects.  PRESENT receives those
objects and ROOT and returns completion candidates."
  (lambda (sink)
    (let ((candidates initial)
          (input "")
          session)
      (lambda (action)
        (pcase action
          ('setup
           (funcall sink action)
           (setq session
                 (consult-jj--candidate-session-create
                  :root (file-name-as-directory (expand-file-name root))
                  :view view
                  :tier tier
                  :buffer (current-buffer)
                  :collect collect
                  :present present
                  :replace
                  (lambda (replacement)
                    (setq candidates replacement)
                    (consult-jj--replace-live-candidates
                     sink candidates input))))
           (push session consult-jj--candidate-sessions)
           nil)
          ('destroy
           (setq consult-jj--candidate-sessions
                 (delq session consult-jj--candidate-sessions))
           (funcall sink action))
          ((pred stringp)
           (setq input action)
           (consult-jj--replace-live-candidates sink candidates input))
          (_ (funcall sink action)))))))

(defun consult-jj--refresh-candidate-sessions (root)
  "Refresh every registered candidate session under ROOT.
Source collection is shared by sessions with equal collector and tier
interfaces."
  (setq consult-jj--candidate-sessions
        (cl-delete-if-not
         (lambda (session)
           (buffer-live-p
            (consult-jj--candidate-session-buffer session)))
         consult-jj--candidate-sessions))
  (let ((sessions
         (cl-remove-if-not
          (lambda (session)
            (and (equal (consult-jj--candidate-session-root session) root)
                 (or
                  (consult-jj--candidate-session-collect session)
                  (assq
                   (consult-jj--candidate-session-view session)
                   consult-jj--candidate-session-adapters))))
          consult-jj--candidate-sessions))
        collected)
    (dolist (session sessions)
      (let* ((adapter
              (alist-get
               (consult-jj--candidate-session-view session)
               consult-jj--candidate-session-adapters))
             (collect
              (or (consult-jj--candidate-session-collect session)
                  (car adapter)))
             (present
              (or (consult-jj--candidate-session-present session)
                  (cdr adapter)))
             (key
              (list
               collect
               (consult-jj--candidate-session-tier session)))
             (cached (assoc key collected))
             (objects
              (if cached
                  (cdr cached)
                (let ((value
                       (funcall
                        collect
                        root
                        (consult-jj--candidate-session-tier session))))
                  (push (cons key value) collected)
                  value)))
             (candidates
              (funcall
               present
               objects root)))
        (with-current-buffer
            (consult-jj--candidate-session-buffer session)
          (funcall
           (consult-jj--candidate-session-replace session)
           candidates)
          (run-hooks 'consult-jj-candidate-session-refreshed-hook))))))

(defun consult-jj--refresh-candidate-sessions-once (root)
  "Refresh candidate sessions under ROOT once in the current context."
  (if consult-jj--candidate-refresh-context
      (unless (car consult-jj--candidate-refresh-context)
        (setcar consult-jj--candidate-refresh-context t)
        (consult-jj--refresh-candidate-sessions root))
    (consult-jj--refresh-candidate-sessions root)))

(defun consult-jj--replace-live-candidates (sink candidates input)
  "Replace SINK contents with CANDIDATES matching INPUT."
  (funcall sink 'flush)
  (when-let ((matching
              (consult-jj--filter-live-candidates candidates input)))
    (funcall sink matching))
  (funcall sink 'refresh))

(defun consult-jj--filter-live-candidates (candidates input)
  "Return CANDIDATES matching Consult INPUT."
  (pcase-let ((`(,regexps . ,highlight)
               (consult--compile-regexp
                input 'emacs completion-ignore-case)))
    (if regexps
        (let* ((completion-regexp-list regexps)
               (matching (all-completions "" candidates)))
          (cl-loop for candidate in-ref matching
                   do (funcall highlight
                               (setf candidate (copy-sequence candidate))))
          matching)
      (copy-sequence candidates))))

(provide 'consult-jj-session)
;;; consult-jj-session.el ends here
