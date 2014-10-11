;;; ghc-mode.el --- Minor mode for enabling GHC interactions.

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

(defvar ghc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'ghc/load)
    (define-key map (kbd "C-c C-t") 'ghc/type-at)
    (define-key map (kbd "M-.") 'ghc/goto-def)
    (define-key map (kbd "C-c C-i") 'ghc/info)
    (define-key map (kbd "C-c M-:") 'ghc/eval)
    map)
  "Keymap for using ghc-mode.")

;;;###autoload
(define-minor-mode ghc-mode
  "Minor mode for enabling ghc-server interaction."
  :lighter " GHC"
  :keymap ghc-mode-map)

(provide 'ghc-mode)
