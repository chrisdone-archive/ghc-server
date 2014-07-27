;;; ghc-con.el --- Connections and requests.

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

(defstruct ghc-con
  "A request handler."
  state cmd filter complete error)

(defvar ghc-con-log-p
  nil
  "Log requests/replies?")

(defvar ghc-con
  nil
  "Current buffer-local connection.")

(defvar ghc-con-number
  0
  "A unique request number counter.")

(defvar ghc-con-requests
  (make-hash-table)
  "Mapping from request ids to requests.")

(defvar ghc-con-buffers
  (make-hash-table)
  "Mapping from process ids to string buffers.")

(defun ghc-con-send (p request)
  "Send a command request and handle the results."
  (let ((rid (setq ghc-con-number (1+ ghc-con-number))))
    (puthash rid request ghc-con-requests)
    (let ((msg (replace-regexp-in-string
                "\n"
                "\\\\n"
                (format "%S" `(request ,rid
                                       ,(ghc-con-cmd request))))))
      (when ghc-con-log-p
        (message "-> %s" msg))
      (process-send-string
       p
       (concat msg "\n")))))

(defun ghc-con-process-sentinel (p sig)
  "Handles connection events."
  (cond ((string= sig "open\n")
         (message "Connected to GHC server."))
        ((string-match "^failed " sig)
         (message "Failed to connect to GHC server."))
        ((string= sig "deleted\n")
         (message "Connection to GHC server deleted."))
        (t (message "%S" sig))))

(defun ghc-con-process-filter (p data)
  "Handles incoming data."
  (let* ((pid (process-id p))
         (buffer (concat (or (gethash pid ghc-con-buffers) "") data))
         (parts (split-string buffer "\n"))
         (lines (delete "" (butlast parts)))
         (remainder (car (last parts))))
    (dolist (line lines)
      (let ((response (read line)))
        (case (car response)
          (response
           (let* ((rid (cadr response))
                  (request (gethash rid ghc-con-requests)))
             (if request
                 (ghc-con-payload rid request (caddr response))
               (message "Bogus result for non-existant request from server: %S" response))))
          (t (message "Bogus line from server: %S" response)))))
    (puthash pid remainder ghc-con-buffers)))

(defun ghc-con-payload (rid request payload)
  "Handle the final payload, calling appropriate handlers."
  (let ((cmd (ghc-con-cmd request))
        (filter (ghc-con-filter request))
        (complete (ghc-con-complete request))
        (error (ghc-con-error request)))
    (when ghc-con-log-p
      (message "<- %S"
               (list (car payload)
                     rid
                     (cadr payload))))
    (case (car payload)
      (result
       (if filter
           (apply filter (list request (cadr payload)))
         (message "Partial results are not supported by this command %S: %S"
                  cmd payload)))
      (end-result
       (remhash rid ghc-con-requests)
       (if complete
           (apply complete (list request (cadr payload)))
         (message "End results are not supported byp this command %S: %S"
                  cmd payload)))
      (error-result
       (remhash rid ghc-con-requests)
       (if error
           (apply error (list request (cadr payload)))
         (message "Error results are not handled by this command: %S\nThe error was: %S"
                  cmd payload)))
      (t
       (message "Bogus result type: %S" payload)))))

(defun ghc-con-create (name)
  "Get or create a connection."
  (let* ((name (format "*ghc-%s*" name))
         (process (get-process name)))
    (if (and process (process-live-p process))
        process
      (progn
        (when process
          (delete-process process))
        (make-network-process
         :name name
         :host (or "localhost" (read-from-minibuffer "Host: " "localhost"))
         :service (or 5233
                      (string-to-number
                       (read-from-minibuffer "Port: " "5233")))
         :nowait t
         :sentinel 'ghc-con-process-sentinel
         :filter 'ghc-con-process-filter)))))

(defun ghc-con-make ()
  "Make a connection and locally assign it."
  (set (make-local-variable 'ghc-con)
       (ghc-con-create "test")))

(defun ghc-con ()
  "Get the current GHC connection."
  (if (bound-and-true-p ghc-con)
      ghc-con
    (ghc-con-make)))

(provide 'ghc-con)
