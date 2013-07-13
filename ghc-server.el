(defvar ghc-request-number
  0
  "A unique request number counter.")

(defstruct ghc-request
  "A request handler."
  state cmd filter complete error)

(defvar ghc-requests
  (make-hash-table)
  "Mapping from request ids to requests.")

(defvar ghc-buffers
  (make-hash-table)
  "Mapping from process ids to string buffers.")

(defun ghc-send (p request)
  "Send a command request and handle the results."
  (let ((rid (setq ghc-request-number (1+ ghc-request-number))))
    (puthash rid request ghc-requests)
    (process-send-string
     p
     (concat (replace-regexp-in-string
              "\n"
              "\\\\n"
              (format "%S" `(request ,rid
                                     ,(ghc-request-cmd request))))
             "\n"))))

(defun ghc-sentinel (p sig)
  "Handles connection events."
  (cond ((string= sig "open\n")
         (message "Connected to GHC server."))
        ((string-match "^failed " sig)
         (message "Failed to connect to GHC server."))
        ((string= sig "deleted\n")
         (message "Connection to GHC server deleted."))
        (t (message "%S" sig))))

(defun ghc-filter (p data)
  "Handles incoming data."
  (let* ((pid (process-id p))
         (buffer (concat (or (gethash pid ghc-buffers) "") data))
         (parts (split-string buffer "\n"))
         (lines (delete "" (butlast parts)))
         (remainder (car (last parts))))
    (dolist (line lines)
      (let ((response (read line)))
        (case (car response)
          ('response
           (let* ((rid (cadr response))
                  (request (gethash rid ghc-requests)))
             (if request
                 (ghc-filter-payload request (caddr response))
               (message "Bogus result for non-existant request from server: %S" response))))
          (t (message "Bogus line from server: %S" response)))))
    (puthash pid remainder ghc-buffers)))

(defun ghc-filter-payload (request payload)
  "Handle the final payload, calling appropriate handlers."
  (let ((cmd (ghc-request-cmd request))
        (filter (ghc-request-filter request))
        (complete (ghc-request-complete request))
        (error (ghc-request-error request)))
    (case (car payload)
      ('result (if filter
                   (apply filter (cons request (cdadr payload)))
                 (message "Partial results are not supported by this command %S: %S"
                          cmd payload)))
      ('end-result
       (remhash rid ghc-requests)
       (if complete
           (apply complete (cons request (cdadr payload)))
         (message "End results are not supported byp this command %S: %S"
                  cmd payload)))
      ('error-result
       (remhash rid ghc-requests)
       (if error
           (apply error (cons request (cdr payload)))
         (message "Error results are not handled by this command: %S\nThe error was: %S"
                  cmd payload)))
      (t (message "Bogus result type: %S" payload)))))

(defun ghc-connect ()
  "Connect to the GHC process."
  (interactive)
  (ghc-process))

(defun ghc-process ()
  "Get or create a connection."
  (let ((process (get-process "*ghc*")))
    (if (and process (process-live-p process))
        process
      (progn
        (when process (delete-process process))
        (make-network-process :name "*ghc*"
                              :host "localhost"
                              :service 5233
                              :nowait t
                              :sentinel 'ghc-sentinel
                              :filter 'ghc-filter)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ping

(defun ghc-ping ()
  "Send a ping command and print delay in milliseconds."
  (interactive)
  (ghc-send (ghc-process)
            (make-ghc-request
             :state nil
             :cmd `(ping ,(round (* 1000 (float-time))))
             :complete 'ghc-pong-complete)))

(defun ghc-pong-complete (request start)
  (let ((end (round (* 1000 (float-time)))))
    (message "Ping reply: %dms" (- end start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load command

(defun ghc-load (target)
  "Load a target (file or module)."
  (interactive (list (read-from-minibuffer "Target: ")))
  (ghc-send (ghc-process)
            (make-ghc-request
             :state nil
             :cmd `(load-target ,target)
             :filter 'ghc-load-target-filter
             :complete 'ghc-load-target-complete)))

(defun ghc-load-target-filter (request result)
  (message "Load filter: %S" result))

(defun ghc-load-target-complete (request result)
  (message "Load: %S" result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type of command

(defun ghc-type (string)
  "Get the type of the given expression."
  (interactive (list (read-from-minibuffer "Type of: "
                                           (haskell-ident-at-point))))
  (ghc-send (ghc-process)
            (make-ghc-request
             :state nil
             :cmd `(type ,string)
             :complete 'ghc-type-complete)))

(defun ghc-type-complete (request result)
  (message ":: %s" result))

(defun ghc-type-error (request error)
  (message "Error: %s" error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kind of command

(defun ghc-kind (string)
  "Get the kind of the given type expression."
  (interactive (list (read-from-minibuffer "Kind of: ")))
  (ghc-send (ghc-process)
            (make-ghc-request
             :state nil
             :cmd `(kind ,string)
             :complete 'ghc-kind-complete)))

(defun ghc-kind-complete (request result)
  (message ":: %s" result))

(defun ghc-kind-error (request error)
  (message "Error: %s" error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info of command

(defun ghc-info (string)
  "Get the info of the given thing."
  (interactive (list (read-from-minibuffer "Info of: ")))
  (ghc-send (ghc-process)
            (make-ghc-request
             :state nil
             :cmd `(info ,string)
             :complete 'ghc-info-complete)))

(defun ghc-info-complete (request result)
  (message "%s" result))

(defun ghc-info-error (request error)
  (message "Error: %s" error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eval command

(defun ghc-eval (string)
  "Evaluate an expression and show the result in the REPL."
  (interactive (list (read-from-minibuffer "Eval: ")))
  (ghc-send (ghc-process)
            (make-ghc-request
             :state (current-buffer)
             :cmd `(eval ,string)
             :complete 'ghc-mode-eval-complete)))

(defun ghc-mode-eval-complete (request result)
  "Handler for a completed eval command."
  (message "Result: %S" result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set

(defun ghc-set (opt)
  "Set some GHC option."
  (interactive (list (read-from-minibuffer "Option: ")))
  (ghc-send (ghc-process)
            (make-ghc-request
             :cmd `(set ,opt)
             :complete 'ghc-set-ok)))

(defun ghc-set-ok (request)
  "Handler for setting options."
  (message "Option set."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure packages

(defun ghc-pkg-conf (opt)
  "Set package configuration file."
  (interactive (list (read-from-minibuffer "Package conf: ")))
  (ghc-send (ghc-process)
            (make-ghc-request
             :cmd `(package-conf ,opt)
             :complete 'ghc-pkg-conf-ok)))

(defun ghc-pkg-conf-ok (request)
  "Handler for setting package conf."
  (message "Set package conf and initialized packages."))
