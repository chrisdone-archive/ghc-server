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

(defun ghc/connect ()
  "Connect if not connected."
  (interactive)
  (ghc-con-make))

(defun ghc/ping ()
  "Ping to check if connection's working."
  (interactive)
  (ghc-cmd-ping))

(defun ghc/load ()
  "Load the current module."
  (interactive)
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
  (interactive (list (read-from-minibuffer "Info of: ")))
  (ghc-cmd-info string))

(defun ghc/kind (string)
  "Get the kind of the given type expression."
  (interactive (list (read-from-minibuffer "Kind of: ")))
  (ghc-cmd-kind string))

(defun ghc/type (string)
  "Get the type of the given type expression."
  (interactive (list (read-from-minibuffer "Type of: ")))
  (ghc-cmd-type string))

(provide 'ghc)
