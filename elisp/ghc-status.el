;;; ghc-status.el --- Project status view.

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

(require 'ghc-repl)
(require 'ghc-session)
(require 'ghc-msgs)

(define-derived-mode ghc-status-mode help-mode "GHC-Status"
  "Major mode for viewing project status from GHC."
  (setq buffer-read-only t)
  (ghc-status-revert))

(define-key ghc-status-mode-map (kbd "g") 'ghc-status-revert)
(define-key ghc-status-mode-map (kbd "G") 'magit-status)
(define-key ghc-status-mode-map (kbd "r") 'ghc-status-repl)
(define-key ghc-status-mode-map (kbd "c") 'ghc-status-cabal)
(define-key ghc-status-mode-map (kbd "m") 'ghc-status-messages)
(define-key ghc-status-mode-map (kbd "l") 'ghc-status-log)

(defun ghc-status-log ()
  (interactive)
  (switch-to-buffer (format "*ghc-server:%s*" (ghc-session-name (ghc-session)))))

(defun ghc-status-cabal ()
  "Open the .cabal file."
  (interactive)
  (find-file (ghc-session-find-cabal-file)))

(defun ghc-status-messages ()
  "Open the messages buffer."
  (interactive)
  (let ((default-directory (ghc-session-dir (ghc-session))))
    (switch-to-buffer (ghc-msgs-buffer (ghc-session-name (ghc-session))))))

(defun ghc-status-repl ()
  "Start the REPL."
  (interactive)
  (let ((default-directory (ghc-session-dir ghc-session)))
    (switch-to-buffer (ghc-repl-buffer (ghc-session-name ghc-session)))))

(defun ghc-status-buffer (name)
  "From a session NAME return a status buffer."
  (let* ((name (format "*ghc-status:%s*" name))
         (buffer (get-buffer name)))
    (or buffer
        (setq buffer
              (with-current-buffer (get-buffer-create name)
                (ghc-status-mode)
                (current-buffer))))
    buffer))

(defun ghc-status-refresh ()
  "Refresh the status buffer, open it if it's not open."
  (let ((default-directory (ghc-session-dir (ghc-session))))
    (with-current-buffer (ghc-status-buffer (ghc-session-name (ghc-session)))
      (ghc-status-revert))))

(defun ghc-status-revert ()
  "Revert status."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((s (ghc-session)))
      (insert (propertize "Name:         " 'face 'font-lock-keyword)
              (ghc-session-name s)
              "\n")
      (insert (propertize "Directory:    " 'face 'font-lock-keyword)
              (ghc-session-dir s)
              "\n")
      (if (ghc-session-con s)
          (insert (propertize "Connection:   " 'face 'font-lock-keyword)
                  (if (eq 'open (process-status (ghc-session-con s)))
                      (apply 'format "%s:%d" (process-contact (ghc-session-con s)))
                    (propertize "Disconnected" 'face 'font-lock-warning-face))
                  "\n")
        (insert (propertize "Connection:   " 'face 'font-lock-keyword)
                "—"
                "\n"))
      (insert (propertize "Status:       " 'face 'font-lock-keyword)
              (ghc-let-if (status (ghc-session-status s))
                          (ecase (car status)
                            (ok (if (= 0 (cadr status))
                                    "OK"
                                  (concat "OK "
                                          (propertize (format "(%d warnings)" (cadr status))
                                                      'face 'compilation-warning))))
                            (compile-error (propertize "Compile error"
                                                       'face 'compilation-error)))
                          "?")
              "\n"))
    (insert "\n")
    (insert (propertize "Keys:         " 'face 'font-lock-keyword)
            (propertize "g" 'face 'font-lock-keyword)
            " - refresh, "
            (propertize "r" 'face 'font-lock-keyword)
            " - repl, "
            (propertize "m" 'face 'font-lock-keyword)
            " - messages\n              "
            (propertize "b" 'face 'font-lock-keyword)
            " - build, "
            (propertize "c" 'face 'font-lock-keyword)
            " - open .cabal file, "
            (propertize "G" 'face 'font-lock-keyword)
            " - magit")
    (insert "\n")
    (goto-char (point-min))))

(provide 'ghc-status)
