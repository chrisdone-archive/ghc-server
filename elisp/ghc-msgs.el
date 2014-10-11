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

(require 'ghc-macros)

(require 'dired)
(require 'cl)

(defface ghc-msgs-current-span
  '((((class color) (min-colors 88) (background dark))
     :background "#333")
    (((class color) (min-colors 88) (background light))
     :background "lightgoldenrod2")
    (t :background "gray"))
  "Basic face for highlighting the current message."
  :group 'ghc)

(define-derived-mode ghc-msgs-mode help-mode "GHC-Messages"
  "Major mode for viewing compile messages from GHC."
  (setq buffer-read-only t)
  (setq next-error-last-buffer (current-buffer))
  (setq next-error-function 'ghc-msgs-next-error))

(defvar ghc-msgs-current-overlay
  nil
  "Currently selected error overlay. Buffer local.")

(define-key ghc-msgs-mode-map (kbd "n") 'ghc-msgs-goto-next-error)
(define-key ghc-msgs-mode-map (kbd "p") 'ghc-msgs-goto-prev-error)
(define-key ghc-msgs-mode-map (kbd "g") nil)
(define-key ghc-msgs-mode-map (kbd "RET") 'ghc-msgs-jump-to-error)

(defun ghc-msgs-next-error (arg reset)
  "Jump to the next error, called by compilation's library."
  (loop for i from 1 to arg
        do (ghc-msgs-goto-next-error reset))
  (ghc-msgs-jump-to-error))

(defun ghc-msgs-jump-to-error (&optional no-jump)
  "Jump to the error at point."
  (interactive)
  (ghc-let-when
   (span (get-text-property (point) 'span))
   (if no-jump
       (save-selected-window
         (ghc-msgs-jump-to-span span))
     (ghc-msgs-jump-to-span span))))

(defun ghc-msgs-jump-to-span (span)
  "Jump to the given span."
  (let ((original (current-buffer)))
    (when (ghc-msgs-find-buffer-of-file (nth 0 span))
      (ghc-msgs-goto-line-col (nth 1 span)
                              (nth 2 span))
      (with-current-buffer original
        (ghc-msgs-focus-span))
      (remove-overlays (point-min) (point-max) 'ghc-msgs-next-error t)
      (let ((o (make-overlay (point)
                             (save-excursion
                               (ghc-msgs-goto-line-col (nth 3 span)
                                                       (nth 4 span))
                               (point)))))
        (overlay-put o 'ghc-msgs-next-error t)
        (overlay-put o 'priority 999)
        (overlay-put o 'face 'ghc-msgs-current-span)
        (sit-for 0.5)
        (delete-overlay o)))))

(defun ghc-msgs-goto-line-col (line col)
  "Jump to the given LINE and COL."
  (goto-char (point-min))
  (forward-line (1- line))
  (goto-char (+ (line-beginning-position)
                (1- col))))

(defun ghc-msgs-focus-span ()
  "Focus the current span."
  (ghc-let-when
   (o ghc-msgs-current-overlay)
   (delete-overlay o))
  (let ((o (make-overlay (get-text-property (point) 'start-marker)
                         (get-text-property (point) 'end-marker))))
    (overlay-put o 'face 'ghc-msgs-current-span)
    (setq ghc-msgs-current-overlay o)))

(defun ghc-msgs-find-buffer-of-file (filename)
  "Find or make a buffer of the given file and switch to it. If
  the buffer's already visible in the frame, switch move focus to
  that."
  (let ((expanded (expand-file-name filename)))
    (ghc-let-if
     (window (car (remove-if-not
                   (lambda (window)
                     (ghc-let-when (name (buffer-file-name (window-buffer window)))
                                   (string= name expanded)))
                   (window-list))))
     (select-window window)
     (find-file-other-window filename))))

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
                (goto-char start-marker))))))))
  (ghc-msgs-jump-to-error t))

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
            (goto-char dont-move))))))
  (ghc-msgs-jump-to-error t))

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
        (format "%s:"
                (dired-make-relative file dir))
        'face (list 'underline
                    (ecase severity
                      (error 'compilation-error)
                      (warning 'compilation-warning-face)))
        'span (list file line-start col-start line-end col-end)
        'end-marker end-marker
        'start-marker start-marker))
      (insert
       (propertize
        (format "%d:%d-%d:%d"
                line-start col-start
                line-end col-end)
        'face '(compilation-line-number underline)
        'span (list file line-start col-start line-end col-end)
        'end-marker end-marker
        'start-marker start-marker))
      (insert
       (propertize
        (format ":\n%s\n"
                msg)
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
