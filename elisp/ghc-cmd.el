;;; ghc-cmd.el --- Commands.

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ghc-con)
(require 'ghc-msgs)

(defun ghc-cmd-ping ()
  "Send a ping command and print delay in milliseconds."
  (ghc-con-send
   (ghc-con)
   (make-ghc-con
    :state nil
    :cmd `(ping ,(round (* 1000 (float-time))))
    :complete 'ghc-cmd-pong-complete)))

(defun ghc-cmd-pong-complete (request result)
  (ecase (car result)
    (pong
     (let ((start (nth 1 result))
           (end (round (* 1000 (float-time)))))
       (message "Ping reply: %dms" (- end start))))))

(defun ghc-cmd-set (opt)
  "Set some GHC option."
  (ghc-con-send
   (ghc-con)
   (make-ghc-con
    :cmd `(set ,opt)
    :complete 'ghc-cmd-set-ok)))

(defun ghc-cmd-set-ok (request _)
  "Handler for setting options."
  (message "Option set."))

(defun ghc-cmd-load (target)
  "Load a target (file or module)."
  (let ((session (ghc-session)))
    (let ((default-directory (ghc-session-dir session)))
      (with-current-buffer (ghc-msgs-buffer (ghc-session-name session))
        (ghc-msgs-clear)))
    (ghc-con-send
     (ghc-con)
     (make-ghc-con
      :state (ghc-session)
      :cmd `(load-target ,target)
      :filter 'ghc-cmd-load-target-filter
      :complete 'ghc-cmd-load-target-complete
      :error 'ghc-cmd-load-target-error))))

(defun ghc-cmd-load-target-filter (request result)
  (let ((session (ghc-con-state request)))
    (ecase (car result)
      (log-result
       (with-current-buffer (ghc-msgs-buffer (ghc-session-name session))
         (ghc-msgs-insert
          (ghc-session-dir session)
          (elt (nth 2 result) 0)
          (elt (nth 2 result) 1)
          (elt (nth 2 result) 3)
          (elt (nth 2 result) 2)
          (elt (nth 2 result) 4)
          (nth 3 result)
          (nth 1 result)))))))

(defun ghc-cmd-load-target-complete (request result)
  (ecase (car result)
    (load-result
     (ecase (nth 1 result)
       (failed (message "Loading module failed."))
       (succeeded (message "OK."))))))

(defun ghc-cmd-load-target-error (request result)
  (message "Load error: %s" result))

(defun ghc-cmd-eval (string)
  "Evaluate an expression and show the result in the REPL."
  (ghc-con-send
   (ghc-con)
   (make-ghc-con
    :state (current-buffer)
    :cmd `(eval ,string)
    :complete 'ghc-cmd-eval-complete
    :filter 'ghc-cmd-eval-filter
    :error 'ghc-cmd-eval-error)))

(defun ghc-cmd-eval-filter (request type)
  "Handler for a completed eval command."
  (ecase (car type)
    (type-result
     (message ":: %s" (cadr type)))
    (eval-import
     (message "Imported, context:\n%s"
              (mapconcat 'identity
                         (cadr type)
                         "\n")))
    (eval-stderr
     (message "Stderr: %s" (cadr type)))
    (eval-stdout
     (message "Stdout: %s" (cadr type)))))

(defun ghc-cmd-eval-complete (request result)
  "Handler for a completed eval command."
  (ecase (car result)
    (unit
     (message "Completed."))
    (eval-result
     (message "Eval result: %s" (cadr result)))
    (decl-result
     (message "Declared names: %s"
              (mapconcat 'identity
                         (cadr result)
                         ", ")))))

(defun ghc-cmd-eval-error (request error)
  "Handler for a completed eval command."
  (message "Evaluation error: %s" (replace-regexp-in-string "\n" " " error)))

(defun ghc-cmd-info (string)
  "Get the info of the given thing."
  (ghc-con-send (ghc-con)
                (make-ghc-con
                 :state nil
                 :cmd `(info ,string)
                 :complete 'ghc-cmd-info-complete
                 :error 'ghc-cmd-info-error)))

(defun ghc-cmd-info-complete (request result)
  (message "%s" (cadr result)))

(defun ghc-cmd-info-error (request error)
  (message "Info error: %s" error))

(defun ghc-cmd-type (string)
  "Get the type of the given expression."
  (ghc-con-send (ghc-con)
                (make-ghc-con
                 :state nil
                 :cmd `(type ,string)
                 :complete 'ghc-cmd-type-complete
                 :error 'ghc-cmd-type-error)))

(defun ghc-cmd-type-complete (request result)
  (message "Type: %s" (cadr result)))

(defun ghc-cmd-type-error (request error)
  (message "Type query error: %s" error))

(defun ghc-cmd-kind (string)
  "Get the kind of the given type expression."
  (interactive (list (read-from-minibuffer "Kind of: ")))
  (ghc-con-send (ghc-con)
                (make-ghc-con
                 :state nil
                 :cmd `(kind ,string)
                 :complete 'ghc-cmd-kind-complete
                 :error 'ghc-cmd-kind-error)))

(defun ghc-cmd-kind-complete (request result)
  (message "Kind: %s" (cadr result)))

(defun ghc-cmd-kind-error (request error)
  (message "Kind query error: %s" error))

(provide 'ghc-cmd)
