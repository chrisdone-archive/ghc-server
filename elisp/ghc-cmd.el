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
(require 'ghc-session)
(require 'ghc-msgs)
(require 'ghc-status)
(require 'ghc-string)

(defun ghc-cmd-ping ()
  "Send a ping command and print delay in milliseconds."
  (ghc-con-send
   (ghc-con)
   (make-ghc-req
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
   (make-ghc-req
    :cmd `(set ,opt)
    :complete 'ghc-cmd-set-ok)))

(defun ghc-cmd-set-ok (request _)
  "Handler for setting options."
  (message "Option set."))


(defun ghc-cmd-cd (dir)
  "Set some GHC option."
  (ghc-con-send
   (ghc-con)
   (make-ghc-req
    :cmd `(cd ,dir)
    :complete 'ghc-cmd-cd-ok)))

(defun ghc-cmd-cd-ok (request _)
  "Handler for setting options."
  (message "Changed directory."))


(defun ghc-cmd-load (target)
  "Load a target (file or module)."
  (let ((session (ghc-session)))
    (let ((default-directory (ghc-session-dir session)))
      (with-current-buffer (ghc-msgs-buffer (ghc-session-name session))
        (ghc-msgs-clear)))
    (ghc-con-send
     (ghc-con)
     (make-ghc-req
      :state (ghc-session)
      :cmd `(load-target ,target)
      :filter 'ghc-cmd-load-target-filter
      :complete 'ghc-cmd-load-target-complete
      :error 'ghc-cmd-load-target-error))))

(defun ghc-cmd-load-target-filter (request result)
  (let ((session (ghc-req-state request)))
    (ecase (car result)
      (msg
       (with-current-buffer (ghc-msgs-buffer (ghc-session-name session))
         (let ((span (nth 2 result)))
           (cond
            ((listp span)
             (ghc-msgs-insert
              (ghc-session-dir session)
              (elt span 0)
              (elt span 1)
              (elt span 3)
              (elt span 2)
              (elt span 4)
              (nth 3 result)
              (nth 1 result)))
            (t (message (nth 3 result))))))))))

(defun ghc-cmd-load-target-complete (request result)
  (ecase (car result)
    (failed
     (setf (ghc-session-status (ghc-req-state request))
           (list 'compile-error))
     (message "Loading module failed.")
     (ghc-status-refresh))
    (succeeded
     (cond
      ((= 0 (nth 1 result))
       (setf (ghc-session-status (ghc-req-state request))
             (list 'ok 0))
       (message "OK."))
      (t
       (setf (ghc-session-status (ghc-req-state request))
             (list 'ok (nth 0 result)))
       (message "OK, %d warnings." (nth 0 result))))

     (ghc-status-refresh))))

(defun ghc-cmd-load-target-error (request result)
  (message "Load error: %s" (ghc-string-chomp result)))

(defun ghc-cmd-eval (string)
  "Evaluate an expression and show the result in the REPL."
  (ghc-con-send
   (ghc-con)
   (make-ghc-req
    :state (current-buffer)
    :cmd `(eval ,string)
    :complete 'ghc-cmd-eval-complete
    :filter 'ghc-cmd-eval-filter
    :error 'ghc-cmd-eval-error)))

(defun ghc-cmd-eval-filter (request type)
  "Handler for a completed eval command."
  (ecase (car type)
    (msg
     (message "Ignoring log result in eval."))
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
     (message "Eval result: %s" (ghc-string-chomp (cadr result))))
    (decl-result
     (message "Declared names: %s"
              (mapconcat 'identity
                         (cadr result)
                         ", ")))))

(defun ghc-cmd-eval-error (request error)
  "Handler for a completed eval command."
  (message "Evaluation error: %s"
           (ghc-string-chomp (replace-regexp-in-string "\n" " " error))))

(defun ghc-cmd-info (string)
  "Get the info of the given thing."
  (ghc-con-send (ghc-con)
                (make-ghc-req
                 :state nil
                 :cmd `(info ,string)
                 :complete 'ghc-cmd-info-complete
                 :error 'ghc-cmd-info-error)))

(defun ghc-cmd-info-complete (request result)
  (message "%s" (ghc-string-chomp (cadr result))))

(defun ghc-cmd-info-error (request error)
  (message "Info error: %s" error))

(defun ghc-cmd-type (string insert)
  "Get the type of the given expression."
  (ghc-con-send (ghc-con)
                (make-ghc-req
                 :state (when insert
                          (cons (point-marker)
                                string))
                 :cmd `(type-of ,string)
                 :complete 'ghc-cmd-type-complete
                 :error 'ghc-cmd-type-error)))

(defun ghc-cmd-type-at (filename string start-line start-col end-line end-col insert-after)
  "Get the type of the given expression."
  (ghc-con-send (ghc-con)
                (make-ghc-req
                 :state (when insert-after
                          (cons (save-excursion (goto-char (line-beginning-position))
                                                (point-marker))
                                string))
                 :cmd `(type-at ,filename
                                ,string
                                ,start-line ,start-col
                                ,end-line ,end-col)
                 :complete 'ghc-cmd-type-complete
                 :error 'ghc-cmd-type-error)))

(defun ghc-cmd-type-complete (request result)
  "Handle type info request completion."
  (let ((marker-and-ident (ghc-req-state request)))
    (if marker-and-ident
        (save-excursion
          (goto-char (car marker-and-ident))
          (insert (cdr marker-and-ident) " :: " (cadr result) "\n"))
      (message "Type: %s" (ghc-cmd-fontify-as-mode (ghc-string-chomp result)
                                                   'haskell-mode)))))

(defun ghc-cmd-type-error (request error)
  (message "Type query error: %s"
           (propertize (ghc-string-chomp error) 'face 'compilation-error)))

(defun ghc-cmd-kind (string)
  "Get the kind of the given type expression."
  (interactive (list (read-from-minibuffer "Kind of: ")))
  (ghc-con-send (ghc-con)
                (make-ghc-req
                 :state nil
                 :cmd `(kind-of ,string)
                 :complete 'ghc-cmd-kind-complete
                 :error 'ghc-cmd-kind-error)))

(defun ghc-cmd-kind-complete (request result)
  (message "Kind: %s" (ghc-cmd-fontify-as-mode result 'haskell-mode)))

(defun ghc-cmd-kind-error (request error)
  (message "Kind query error: %s" error))

(defun ghc-cmd-goto-loc (filename string start-line start-col end-line end-col)
  "Go to the location of the given name at location."
  (ghc-con-send (ghc-con)
                (make-ghc-req
                 :state nil
                 :cmd `(loc-at ,filename
                               ,string
                               ,start-line ,start-col
                               ,end-line ,end-col)
                 :complete 'ghc-cmd-goto-loc-complete
                 :error 'ghc-cmd-goto-loc-error)))

(defun ghc-cmd-goto-loc-complete (request result)
  "Jump to the file and line/col."
  (destructuring-bind (fp sl el sc ec) result
    (find-file fp)
    (goto-char (point-min))
    (forward-line (1- sl))
    (forward-char (1- sc))))

(defun ghc-cmd-goto-loc-error (request error)
  "Error doing stuff."
  (message "%s" (propertize error
                            'face 'compilation-error)))

(defun ghc-cmd-fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (font-lock-fontify-buffer)
    (buffer-substring (point-min) (point-max))))

(defun ghc-cmd-uses (filename string start-line start-col end-line end-col insert-after)
  "Get uses of the given identifier."
  (ghc-con-send (ghc-con)
                (make-ghc-req
                 :state (when insert-after
                          (cons (save-excursion (goto-char (line-beginning-position))
                                                (point-marker))
                                string))
                 :cmd `(uses ,filename
                             ,string
                             ,start-line ,start-col
                             ,end-line ,end-col)
                 :complete 'ghc-cmd-uses-complete
                 :error 'ghc-cmd-uses-error)))

(defun ghc-cmd-uses-complete (request result)
  "Handle type info request completion."
  (message "Type: %s" (ghc-cmd-fontify-as-mode (ghc-string-chomp (format "%S" result))
                                               'haskell-mode)))

(defun ghc-cmd-uses-error (request error)
  (message "Type uses error: %s"
           (propertize (ghc-string-chomp error) 'face 'compilation-error)))

(provide 'ghc-cmd)
