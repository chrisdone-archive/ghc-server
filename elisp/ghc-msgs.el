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

(require 'dired)
(require 'cl)

(define-derived-mode ghc-msgs-mode help-mode "GHC-Messages"
  "Major mode for viewing compile messages from GHC."
  (setq buffer-read-only t)
  (setq next-error-last-buffer (current-buffer))
  (setq next-error-function 'ghc-msgs-next-error))

(define-key ghc-msgs-mode-map (kbd "n") 'ghc-msgs-goto-next-error)
(define-key ghc-msgs-mode-map (kbd "p") 'ghc-msgs-goto-prev-error)
(define-key ghc-msgs-mode-map (kbd "RET") 'ghc-msgs-jump-to-error)

(defun ghc-msgs-next-error (arg reset)
  "Jump to the next error, called by compilation's library."
  (loop for i from 1 to arg
        do (ghc-msgs-goto-next-error reset))
  (ghc-msgs-jump-to-error))

(defun ghc-msgs-jump-to-error ()
  "Jump to the error at point."
  (interactive)
  (let ((span (get-text-property (point) 'span)))
    (when span
      (when (find-file (nth 0 span))
        (goto-char (point-min))
        (forward-line (1- (nth 1 span)))
        (goto-char (+ (line-beginning-position)
                      (1- (nth 2 span))))))))

(defun ghc-msgs-goto-prev-error (&optional reset)
  "Jump to the next error. Cycle to the start if RESET is
  specified."
  (interactive)
  (cond
   ((= (line-beginning-position)
       (line-end-position))
    (forward-char -1)
    (let ((start-marker (get-text-property (point) 'start-marker)))
      (when start-marker (goto-char start-marker))))
   (t (let ((start-marker (get-text-property (point) 'start-marker)))
        (when start-marker
          (goto-char start-marker)
          (unless (= (point) (point-min))
            (forward-char -1)
            (let ((start-marker (get-text-property (point) 'start-marker)))
              (when start-marker
                (goto-char start-marker)))))))))

(defun ghc-msgs-goto-next-error (&optional reset)
  "Jump to the next error. Cycle to the start if RESET is
  specified."
  (interactive)
  (let ((end-marker (get-text-property (point) 'end-marker)))
    (when end-marker
      (let ((dont-move (point)))
        (goto-char end-marker)
        (when (not (get-text-property (point) 'end-marker))
          (if reset
              (goto-char (point-min))
            (goto-char dont-move)))))))

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
    (setq next-error-last-buffer (current-buffer))
    (erase-buffer)))

(defun ghc-msgs-insert (dir file line-start col-start line-end col-end msg severity)
  "Insert an error message into the buffer."
  (setq next-error-last-buffer (current-buffer))
  (let ((inhibit-read-only t)
        (end-marker (make-marker))
        (start-marker (make-marker)))
    (let ((start (point)))
      (insert
       (propertize
        (format "%s:%d:%d-%d:%d:\n%s\n"
                (dired-make-relative file dir)
                line-start col-start
                line-end col-end
                msg)
        'face (ecase severity
                (error 'compilation-error)
                (warning 'compilation-warning-face))
        'span (list file line-start col-start line-end col-end)
        'end-marker end-marker
        'start-marker start-marker))
      (set-marker start-marker start)
      (set-marker end-marker (point))
      (save-excursion
        (goto-char start)
        (forward-line)
        (indent-rigidly (point) end-marker 2)))))

(provide 'ghc-msgs)
