;;; ghc.el --- Communication with ghc-server.

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
(require 'ghc-cmd)
(require 'ghc-mode)
(require 'ghc-repl)
(require 'ghc-status)
(require 'ghc-ident)
(require 'ghc-process)

(require 'tramp)

(defun ghc/start ()
  "Start a service process."
  (interactive)
  (let ((port (ghc-process-free-port))
        (session (ghc-session)))
    (ghc-process-start
     port
     (ghc-session-dir session)
     (ghc-session-name session))
    (setf (ghc-session-port session)
          port)))

(defun ghc/connect (prompt)
  "Connect if not connected."
  (interactive "P")
  (ghc-con-make prompt))

(defun ghc/disconnect ()
  "Disconnect."
  (interactive)
  (ghc-con-disconnect))

(defun ghc/ping ()
  "Ping to check if connection's working."
  (interactive)
  (ghc-cmd-ping))

(defun ghc/cd (dir)
  "Change ghc working directory."
  (interactive "D")
  (ghc-cmd-cd (expand-file-name dir)))

(defun ghc/load ()
  "Load the current module."
  (interactive)
  (save-buffer)
  (if (eq major-mode 'haskell-mode)
      (ghc-cmd-load (let ((name (file-relative-name
                                 (buffer-file-name)
                                 (ghc-session-dir (ghc-session)))))
                      (if (tramp-tramp-file-p name)
                          (let ((vec (tramp-dissect-file-name name)))
                            (tramp-file-name-localname vec))
                        name)))
    (ghc-cmd-load (read-from-minibuffer "Load: "))))

(defun ghc/set (opt)
  "Set some GHC option e.g. -Wall."
  (interactive (list (read-from-minibuffer "Option: ")))
  (ghc-cmd-set opt))

(defun ghc/eval (string)
  "Evaluate an expression and show the result in the REPL."
  (interactive (list (read-from-minibuffer "Eval: ")))
  (ghc-cmd-eval string))

(defun ghc/info (string)
  "Get the info of the given thing."
  (interactive (list (or (ghc-ident-at-point)
                         (read-from-minibuffer "Info of: "))))
  (ghc-cmd-info string))

(defun ghc/kind (string)
  "Get the kind of the given type expression."
  (interactive (list (read-from-minibuffer "Kind of: ")))
  (ghc-cmd-kind string))

(defun ghc/type (prefix-arg)
  "Get the type of the given type expression."
  (interactive "P")
  (let ((string (or (ghc-ident-at-point)
                    (read-from-minibuffer "Type of: "))))
    (ghc-cmd-type string prefix-arg)))

(defun ghc/type-at (prefix-arg)
  "Get the type of the identifier at point, or at region."
  (interactive "P")
  (let ((pos (or (when (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
                 (ghc-ident-pos-at-point)
                 (cons (point)
                       (point)))))
    (when pos
      (save-excursion
        (ghc-cmd-type-at
         (file-relative-name (buffer-file-name) (ghc-session-dir (ghc-session)))
         (buffer-substring-no-properties (car pos)
                                         (cdr pos))
         (progn (goto-char (car pos))
                (line-number-at-pos))
         (current-column)
         (progn (goto-char (cdr pos))
                (line-number-at-pos))
         (current-column)
         prefix-arg)))))

(defun ghc/uses (prefix-arg)
  "Display the uses of the identifer at point."
  (interactive "P")
  (let ((pos (or (when (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
                 (ghc-ident-pos-at-point)
                 (cons (point)
                       (point)))))
    (when pos
      (save-excursion
        (ghc-cmd-uses
         (file-relative-name (buffer-file-name) (ghc-session-dir (ghc-session)))
         (buffer-substring-no-properties (car pos)
                                         (cdr pos))
         (progn (goto-char (car pos))
                (line-number-at-pos))
         (current-column)
         (progn (goto-char (cdr pos))
                (line-number-at-pos))
         (current-column)
         prefix-arg)))))

(defun ghc/goto-def ()
  "Go to the definition of the identifer at point."
  (interactive)
  (let ((pos (or (when (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
                 (ghc-ident-pos-at-point)
                 (cons (point)
                       (point)))))
    (when pos
      (save-excursion
        (ghc-cmd-goto-loc
         (file-relative-name (buffer-file-name) (ghc-session-dir (ghc-session)))
         (buffer-substring-no-properties (car pos)
                                         (cdr pos))
         (progn (goto-char (car pos))
                (line-number-at-pos))
         (current-column)
         (progn (goto-char (cdr pos))
                (line-number-at-pos))
         (current-column))))))

(defun ghc/msgs ()
  "Create/show the messages buffer."
  (interactive)
  (let ((default-directory (ghc-session-dir (ghc-session))))
    (switch-to-buffer-other-window (ghc-msgs-buffer (ghc-session-name (ghc-session))))))

(defun ghc/status ()
  "Create/show the status buffer."
  (interactive)
  (let ((default-directory (ghc-session-dir (ghc-session))))
    (switch-to-buffer-other-window (ghc-status-buffer (ghc-session-name (ghc-session))))))

(defun ghc/repl ()
  "Create/show the REPL buffer."
  (interactive)
  (let ((default-directory (ghc-session-dir (ghc-session))))
    (switch-to-buffer (ghc-repl-buffer (ghc-session-name (ghc-session))))))

(provide 'ghc)
