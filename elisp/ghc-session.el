;;; ghc-session.el --- Manage directory-specific sessions.

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
(require 'cl)

(defvar ghc-session-startup
  nil
  "A startup function to run when connected.")

(defstruct ghc-session
  "A ghc-server session."
  name con dir port status)

(defmacro ghc-session-migrate (name)
  "Add a new slot to the given session. Used for development."
  `(setq ,name
         (apply 'vector (append (map 'list #'identity ,name)
                                (list nil)))))

(defvar ghc-session
  nil
  "Buffer-local variable for current session.")

(defvar ghc-session-list
  nil
  "A list of all active sessions.")

(defun ghc-session ()
  "Get, or figure out and set, the current session."
  (or (ghc-session-get)
      (ghc-session-bet)
      (ghc-session-set)))

(defun ghc-session-get ()
  "Get the current session."
  ghc-session)

(defun ghc-session-bet ()
  "Take a gamble that the directory will match one of the
  sessions in the session list."
  (ghc-let-when
   (file (ghc-session-find-cabal-file))
   (let ((dir (file-name-directory file)))
     (set (make-local-variable 'ghc-session)
          (car (remove-if-not
                (lambda (s)
                  (string= (ghc-session-dir s)
                           dir))
                ghc-session-list))))))

(defun ghc-session-set ()
  "Set the current session."
  (let* ((file (ghc-session-find-cabal-file))
         (dir (if file
                  (file-name-directory file)
                default-directory))
         (name (if file
                   (ghc-session-unique-name
                    (replace-regexp-in-string "\\.cabal$"
                                              ""
                                              (file-name-nondirectory file)))
                 (ghc-session-unique-name "ghc-server")))
         (session (make-ghc-session :name name :con nil :dir dir)))
    (add-to-list 'ghc-session-list session)
    (set (make-local-variable 'ghc-session)
         session)))

(defun ghc-session-unique-name (name)
  "Generate a unique name by avoiding conflicts with anything in
  `ghc-session-list'."
  (if (remove-if-not (lambda (existing) (string= (ghc-session-name existing) name))
                     ghc-session-list)
      (ghc-session-unique-name (concat name "'"))
    name))

(defun ghc-session-find-cabal-file (&optional dir)
  "Search for package description file upwards starting from DIR.
If DIR is nil, `default-directory' is used as starting point for
directory traversal.  Upward traversal is aborted if file owner
changes.  Uses`haskell-cabal-find-pkg-desc' internally."
  (catch 'found
    (let ((user (nth 2 (file-attributes (or dir default-directory))))
          ;; Abbreviate, so as to stop when we cross ~/.
          (root (abbreviate-file-name (or dir default-directory))))
      ;; Traverse current dir up to root as long as file owner doesn't
      ;; change.
      (while (and root (equal user (nth 2 (file-attributes root))))
        (let ((cabal-file (ghc-session-find-pkg-desc root)))
          (when cabal-file
            (throw 'found cabal-file)))

        (let ((proot (file-name-directory (directory-file-name root))))
          (if (equal proot root) ;; fix-point reached?
              (throw 'found nil)
            (setq root proot))))
      nil)))

(defun ghc-session-find-pkg-desc (dir &optional allow-multiple)
  "Find a package description file in the directory DIR.
Returns nil if none or multiple \".cabal\" files were found.  If
ALLOW-MULTIPLE is non nil, in case of multiple \".cabal\" files,
a list is returned instead of failing with a nil result."
  (let* ((cabal-files
          (remove-if 'file-directory-p
                     (remove-if-not 'file-exists-p
                                    (directory-files dir t ".\\.cabal\\'")))))
    (cond
     ((= (length cabal-files) 1) (car cabal-files)) ;; exactly one candidate found
     (allow-multiple cabal-files) ;; pass-thru multiple candidates
     (t nil))))

(provide 'ghc-session)
