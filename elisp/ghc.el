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
(require 'ghc-ident)

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

(defun ghc/load ()
  "Load the current module."
  (interactive)
  (save-buffer)
  (if (eq major-mode 'haskell-mode)
      (ghc-cmd-load (buffer-file-name))
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

(defun ghc/type (string)
  "Get the type of the given type expression."
  (interactive (list (or (ghc-ident-at-point)
                         (read-from-minibuffer "Type of: "))))
  (ghc-cmd-type string))

(defun ghc/msgs ()
  "Create/show the messages buffer."
  (interactive)
  (let ((default-directory (ghc-session-dir (ghc-session))))
    (switch-to-buffer (ghc-msgs-buffer (ghc-session-name (ghc-session))))))

(defun ghc/repl ()
  "Create/show the REPL buffer."
  (interactive)
  (let ((default-directory (ghc-session-dir (ghc-session))))
    (switch-to-buffer (ghc-repl-buffer (ghc-session-name (ghc-session))))))

(provide 'ghc)
