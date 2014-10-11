;;; ghc-repl.el --- A REPL for GHC.

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

(require 'ring)

(require 'ghc-con)
(require 'ghc-msgs)

(defvar ghc-repl-prompt-start
  nil
  "Marker for the start of the prompt.")

(defvar ghc-repl-prompt-ring
  nil
  "Ring used for the prompt history.")

(define-derived-mode ghc-repl-mode fundamental-mode "GHC-REPL"
  "Major mode for a prompt-based interaction with GHC."
  (set (make-local-variable 'ghc-repl-prompt-start)
       (make-marker))
  (set (make-local-variable 'ghc-repl-prompt-ring)
       (make-ring 512))
  (ghc-repl-prompt))

(define-key ghc-repl-mode-map (kbd "C-a") 'ghc-repl-bol)
(define-key ghc-repl-mode-map (kbd "M-p") 'ghc-repl-history-prev)
(define-key ghc-repl-mode-map (kbd "M-n") 'ghc-repl-history-next)
(define-key ghc-repl-mode-map (kbd "RET") 'ghc-repl-return)
(define-key ghc-repl-mode-map (kbd "C-c C-k") 'ghc-repl-clear)

(defun ghc-repl-history-prev (n)
  "Go back in history."
  (interactive "p")
  (ghc-repl-nav-history n))

(defun ghc-repl-history-next (n)
  "Go forward in history."
  (interactive "p")
  (ghc-repl-nav-history (- n)))

(defun ghc-repl-nav-history (offset)
  "Rotate the history ring and show the top of it."
  (let ((index (ghc-let-if (index (save-excursion
                                    (goto-char ghc-repl-prompt-start)
                                    (get-text-property (point) 'ghc-repl-prompt-index)))
                           (+ index offset)
                           0)))
    (ghc-repl-set-prompt
     (propertize (ring-ref ghc-repl-prompt-ring index)
                 'ghc-repl-prompt-index
                 index))))

(defun ghc-repl-push-history (text)
  "Push a history item onto the ring."
  (ring-insert ghc-repl-prompt-ring text))

(defun ghc-repl-set-prompt (text)
  "Set the text at the prompt."
  (goto-char ghc-repl-prompt-start)
  (delete-region ghc-repl-prompt-start
                 (point-max))
  (insert text))

(defun ghc-repl-bol ()
  "Go to beginning of line."
  (interactive)
  (if (>= (point) ghc-repl-prompt-start)
      (goto-char ghc-repl-prompt-start)
    (if (or (get-text-property (point) 'old-input)
            (get-text-property (1- (point)) 'old-input))
        (goto-char (or (get-text-property (point) 'start-point)
                       (get-text-property (1- (point)) 'start-point)))
      (goto-char (line-beginning-position)))))

(defun ghc-repl-return ()
  "Handle return in the REPL."
  (interactive)
  (if (get-text-property (point) 'old-input)
      (let ((text (buffer-substring-no-properties (get-text-property (point) 'start-point)
                                                  (get-text-property (point) 'end-point))))
        (goto-char (point-max))
        (ghc-repl-clear-prompt)
        (insert text))
    (save-excursion
      (let ((input (buffer-substring-no-properties ghc-repl-prompt-start
                                                   (point-max))))
        (ghc-repl-eval input)))))

(defun ghc-repl-clear-prompt ()
  "Clear the current prompt."
  (delete-region ghc-repl-prompt-start
                 (point-max)))

(defun ghc-repl-clear ()
  "Clear the buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (setq next-error-last-buffer (current-buffer))
    (erase-buffer)
    (ghc-repl-prompt)))

(defun ghc-repl-buffer (name)
  "From a session NAME return a REPL buffer."
  (let* ((name (format "*ghc-repl:%s*" name))
         (buffer (get-buffer name)))
    (or buffer
        (with-current-buffer (get-buffer-create name)
          (ghc-repl-mode)
          (current-buffer)))))

(defun ghc-repl-prompt ()
  "Insert the REPL prompt."
  (let ((inhibit-read-only t))
    (insert
     (propertize "λ>"
                 'face 'font-lock-keyword-face
                 'read-only t)
     (propertize " "
                 'read-only t
                 'rear-nonsticky t))
    (set-marker ghc-repl-prompt-start (point))))

(defun ghc-repl-eval-filter (request type)
  "Handler for a completed eval command."
  (ecase (car type)
    (type-result
     (ghc-repl-complete-prompt)
     (ghc-repl-result (concat ":: " (cadr type))))
    (eval-import
     (message "Imported, context:\n%s"
              (mapconcat 'identity
                         (cadr type)
                         "\n")))
    (log-result
     (message "%s"
              (propertize (nth 3 type) 'face 'compilation-warning)))
    (eval-stdout
     (ghc-repl-complete-prompt)
     (ghc-repl-stdout (cadr type)))))

(defun ghc-repl-complete-prompt ()
  "Complete a finished prompt, make it read-only, re-usable and
  start a new prompt."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (or (= (line-end-position) (line-beginning-position))
                (get-text-property (1- (point)) 'stdout)
                (get-text-property (1- (point)) 'repl-result))
      (put-text-property ghc-repl-prompt-start (point-max)
                         'old-input t)
      (put-text-property ghc-repl-prompt-start (point-max)
                         'start-point (marker-position ghc-repl-prompt-start))
      (put-text-property ghc-repl-prompt-start (point-max)
                         'end-point (point-max))
      (let ((end (point)))
        (insert (propertize "\n" 'prompt-complete-newline t))
        (put-text-property ghc-repl-prompt-start end
                           'read-only t)))))

(defun ghc-repl-eval-complete (request result)
  "Handler for a completed eval command."
  (with-current-buffer (ghc-repl-buffer (ghc-session-name (ghc-req-session request)))
    (ghc-repl-complete-prompt)
    (when result
      (ecase (car result)
        (new-context
         (message "New context: %s" (mapconcat 'identity
                                               (cadr result)
                                               ", ")))
        (eval
         (ghc-repl-result result))
        (decl-resul
         (if (and (consp result)
                  (not (equalp result (list "it"))))
             (ghc-repl-output
              (concat (propertize "Declared names: " 'face 'font-lock-comment-face)
                      (format "%s"
                              (mapconcat (lambda (name)
                                           (propertize name 'face 'font-lock-reference-face))
                                         result
                                         (propertize ", "
                                                     'face 'font-lock-comment-face)))))
           (insert "\n")))))
    (ghc-repl-prompt)))

(defun ghc-repl-result (result)
  "Insert an evaluation result."
  (let ((inhibit-read-only t))
    (unless (and (looking-back "\n")
                 (or (= (line-beginning-position) (line-end-position))
                     (get-text-property (1- (point))
                                        'prompt-complete-newline)
                     (get-text-property (1- (point))
                                        'repl-result)))
      (insert "\n"))
    (insert (propertize (ghc-repl-fontify-as-mode result 'haskell-mode)
                        'repl-result t)
            "\n")))

(defun ghc-repl-stdout (result)
  "Insert stdout output."
  (let ((inhibit-read-only t))
    (insert (propertize result
                        'face 'font-lock-string-face
                        'stdout t))))

(defun ghc-repl-fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (font-lock-fontify-buffer)
    (buffer-substring (point-min) (point-max))))

(defun ghc-repl-output (result)
  "Insert an evaluation output."
  (insert result "\n"))

(defun ghc-repl-eval-error (request error)
  "Handler for a completed eval command."
  (message "%s"
           (propertize (replace-regexp-in-string "\n" " " error)
                       'face
                       'compilation-error))
  (with-current-buffer (ghc-repl-buffer (ghc-session-name (ghc-req-session request)))
    (ghc-repl-complete-prompt)
    (ghc-repl-prompt)))

(defun ghc-repl-eval (string)
  "Evaluate an expression and show the result in the REPL."
  (ghc-repl-push-history string)
  (ghc-con-send
   (ghc-con)
   (make-ghc-req
    :state (current-buffer)
    :cmd `(eval ,string)
    :complete 'ghc-repl-eval-complete
    :filter 'ghc-repl-eval-filter
    :error 'ghc-repl-eval-error)))

(provide 'ghc-repl)
