;;; ghc-macros.el --- Some macros used in the project.

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

(defmacro ghc-let-if (name-expr then else)
  `(let ((,(car name-expr) ,(cadr name-expr)))
     (if ,(car name-expr)
         ,then
       ,else)))

(defmacro ghc-let-when (name-expr then)
  `(let ((,(car name-expr) ,(cadr name-expr)))
     (if ,(car name-expr)
         ,then)))

(provide 'ghc-macros)
