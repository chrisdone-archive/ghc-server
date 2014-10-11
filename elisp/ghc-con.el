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

(require 'ghc-log)
(require 'ghc-session)
(require 'ghc-macros)
(require 'cl)

(defstruct ghc-req
  "A request handler."
  state cmd filter complete error session)

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
    (setf (ghc-req-session request) (ghc-session))
    (puthash rid request ghc-con-requests)
    (let ((msg (replace-regexp-in-string
                "\n"
                "\\\\n"
                (format "%S" `(request ,rid
                                       ,(ghc-req-cmd request))))))
      (ghc-log "%s" msg)
      (process-send-string
       p
       (concat msg "\n")))))

(defun ghc-con-process-sentinel (p sig)
  "Handles connection events."
  (cond ((string= sig "open\n")
         (message "Connected to GHC server!")
         (let ((startup ghc-session-startup))
           (when startup
             (funcall (eval startup)))))
        ((string-match "^failed " sig)
         (message "Failed to connect to GHC server. Run M-x ghc/start to start a local one."))
        ((string= sig "deleted\n")
         (message "Disconnected from GHC server!"))
        (t
         (message "Connection error (%s)"
                  (replace-regexp-in-string "\n" " " sig)))))

(defun ghc-con-process-filter (p data)
  "Handles incoming data."
  (let* ((pid (process-id p))
         (buffer (concat (or (gethash pid ghc-con-buffers) "") data))
         (parts (split-string buffer "\n"))
         (lines (delete "" (butlast parts)))
         (remainder (car (last parts))))
    (dolist (line lines)
      (let ((response (read line)))
        (ghc-log "%s" line)
        (let* ((rid (cadr response))
               (request (gethash rid ghc-con-requests)))
          (if request
              (ghc-con-payload rid request (car response) (caddr response))
            (message "Bogus result for non-existant request from server: %S" response)))))
    (puthash pid remainder ghc-con-buffers)))

(defun ghc-con-payload (rid request type payload)
  "Handle the final payload, calling appropriate handlers."
  (let* ((cmd (ghc-req-cmd request))
         (filter (ghc-req-filter request))
         (complete (ghc-req-complete request))
         (error (ghc-req-error request))
         (session (ghc-req-session request))
         (default-directory (ghc-session-dir session)))
    (message "type: %S" type)
    (case type
      (result
       (if filter
           (apply filter (list request payload))
         (message "Partial results are not supported by this command %S: %S"
                  cmd payload)))
      (end-result
       (remhash rid ghc-con-requests)
       (if complete
           (apply complete (list request payload))
         (message "End results are not supported by this command %S: %S"
                  cmd payload)))
      (error-result
       (remhash rid ghc-con-requests)
       (if error
           (apply error (list request payload))
         (message "Error results are not handled by this command: %S\nThe error was: %S"
                  cmd payload)))
      (t
       (message "Bogus result type: %S" payload)))))

(defun ghc-con-create (name prompt)
  "Get or create a connection."
  (let* ((name (format "*ghc-server:%s*" name))
         (process (get-process name)))
    (if (and process (process-live-p process))
        process
      (progn
        (when process
          (delete-process process))
        (make-network-process
         :name name
         :host (if prompt
                   (read-from-minibuffer "Host: " "localhost")
                 "localhost")
         :service (if prompt
                      (string-to-number
                       (read-from-minibuffer "Port: " "5233"))
                    (ghc-let-if (port (ghc-session-port (ghc-session)))
                                port
                                (error "No port specified. Run M-x ghc/start to start a local server or use C-u M-x ghc/connect to specify a host/port.")))
         :sentinel 'ghc-con-process-sentinel
         :filter 'ghc-con-process-filter)))))

(defun ghc-con-make (&optional prompt)
  "Make a connection and locally assign it."
  (let ((session (ghc-session)))
    (let* ((name (ghc-session-name session))
           (con (ghc-con-create name prompt)))
      (setf (ghc-session-con session) con)
      con)))

(defun ghc-con ()
  "Get the current GHC connection."
  (ghc-let-if (session (ghc-session-get))
              (let ((proc (ghc-session-con session)))
                (if (and proc (process-live-p proc))
                    proc
                  (unless (ghc-con-make nil)
                    (error (concat "Not connected to a server. Run M-x ghc/connect to connect, "
                                   "or ghc/start to start a process.")))))
              (ghc-con-make)))

(defun ghc-con-disconnect ()
  "Disconnect from the server."
  (ghc-let-when (session (ghc-session-get))
                (delete-process (ghc-session-con session))))

(provide 'ghc-con)
