;;; distel-company.el --- Erlang/distel completion backend for company-mode

;; Copyright (C) 2012 Sebastian Weddmark Olsson
;; Copyright (C) 2018 mats cronqvist

;; Author: Sebastian Weddmark Olsson
;;; Commentary:

;; Add `distel-company' to the `company-backends' list in your .emacs.
;; E.g.
;;   (require 'company)
;;   (require 'distel-company)
;;   (add-to-list 'company-backends 'distel-company)
;;

;;; Code:

;;;###autoload

(require 'cl-lib)
(require 'distel)
(require 'erlang)

(defun distel-company (command &optional args &rest ignored)
  "Company-mode for erlang using distel.
This is the company facade, a switch on COMMAND.
What's in ARGS depends on the value of COMMAND."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'distel-company))
    (prefix (distel-company--prefix)) ;"list")
    (candidates (distel-company--rpc 'candidates args))
    (meta (distel-company--rpc 'meta args))
    (annotation (distel-company--rpc 'annotation args))
;;    (post-completion (progn
;;       (run-with-timer 0 nil 'distel-company--post-completion args)))
    (ignore-case nil)
    (no-cache t)
    (duplicates nil)
    (sorted t)
    (_ nil)))

(defvar distel-company-rpc-sync nil
  "Global variable to sync between buffers.")

(defun distel-company--prefix ()
  "Get the prefix we want to complete."
  (let ((id (erlang-get-identifier-at-point)))
    (message "prefix: %s" id)
    (pcase id
      (`(nil ,_ ,mod ,_) mod)
      (`(qualified-function ,mod ,fun nil) (concat mod ":" fun))
      (`(module ,_ ,mod nil) (concat mod ":"))
      (_ 'stop))))

(defun distel-company--rpc (what prefix)
  "Call distel, passing WHAT and PREFIX to the erlang side."
  (setq distel-company-rpc-sync nil)
  (erl-spawn
    (erl-send-rpc (erl-target-node) 'distel 'completions (list what prefix))
    (erl-receive ()
        ((['rex ['ok result]]
          (setq distel-company-rpc-sync result))
         (other
          (message "distel-company: Unexpected reply: %s" other)))))
  (distel-company--wait-for-rpc))

(defun distel-company--wait-for-rpc ()
  "Use global variable distel-company-rpc-sync to sync between buffers."
  (if distel-company-rpc-sync
      distel-company-rpc-sync
    (sit-for 0.1)
    (distel-company--wait-for-rpc)))

(defun &distel-company--rpc-receive ()
  "Receiver loop."

(defun distel-company--post-completion (prefix)
  "If we complete, we might want to complete again (on PREFIX)."
  (company-begin-with (distel-company--rpc 'candidates prefix))
  (let ((this-command 'company-idle-begin))
    (company-post-command)))


(provide 'distel-company)
;;; distel-company.el ends here
