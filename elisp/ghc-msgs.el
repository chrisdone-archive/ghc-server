;;; ghc-msgs.el --- Messages buffer.

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

(define-derived-mode ghc-msgs-mode help-mode "GHC-Messages"
  "Major mode for viewing compile messages from GHC."
  (setq buffer-read-only t))

(defun ghc-msgs-buffer (name)
  "From a session NAME return a messages buffer."
  (let* ((name (format "*ghc-msgs:%s*" name))
         (buffer (get-buffer name)))
    (or buffer
        (with-current-buffer (get-buffer-create name)
          (ghc-msgs-mode)
          (current-buffer)))))

(defun ghc-msgs-clear ()
  "Clear the buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun ghc-msgs-insert (dir file line-start col-start line-end col-end msg)
  "Insert an error message into the buffer."
  (let ((inhibit-read-only t))
    (insert (dired-make-relative file dir)
            ":"
            (format "%d:%d-%d:%d"
                    line-start col-start
                    line-end col-end)
            ":\n"
            msg)))

(provide 'ghc-msgs)
