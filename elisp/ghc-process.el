;;; ghc-process.el --- Start a ghc-server process locally

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

(defun ghc-process-start (port dir name)
  "Start a ghc-process."
  (let ((default-directory dir))
    (with-current-buffer (get-buffer-create (format "*ghc-server:%s*" name))
      (shell (current-buffer))
      (insert "ghc-server --port "
              (format "%d" port))
      (comint-send-input nil t)
      (bury-buffer (current-buffer)))))

(defun ghc-process-free-port ()
  "Return a free (unused) TCP port.

The port is chosen randomly from the ephemeral ports."
  (let* (myserver
         (base 5233)
         (port base))
    (while
        (not
         (processp
          (condition-case sig
              (setq myserver
                    (make-network-process
                     :name "*test-proc*"
                     :server t
                     :nowait 't
                     :host 'local
                     :service port
                     :family 'ipv4))
            (file-error
             (if (equal
                  "Cannot bind server socket address already in use"
                  (mapconcat 'identity (cdr sig) " "))
                 (setq port (+ base (random 5000)))))))))
    (delete-process myserver)
    port))

(provide 'ghc-process)
