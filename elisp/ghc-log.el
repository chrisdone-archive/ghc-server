;;; ghc-log.el --- Logging buffer.

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

(defvar ghc-log-p
  t
  "Log requests/replies?")

(defun ghc-log (fmt &rest args)
  "Log using `message' if `ghc-log-p' is t."
  (when ghc-log-p
    (let* ((name "*ghc-log*")
           (buffer (get-buffer name)))
      (when (not buffer)
        (setq buffer
              (with-current-buffer (get-buffer-create name)
                (emacs-lisp-mode))))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (apply #'format
                       (cons fmt args))
                "\n")))))

(provide 'ghc-log)
