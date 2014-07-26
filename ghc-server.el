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
    (let ((msg (replace-regexp-in-string
                "\n"
                "\\\\n"
                (format "%S" `(request ,rid
                                       ,(ghc-request-cmd request))))))
      (message "-> %s" msg)
      (process-send-string
       p
       (concat msg "\n")))))

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
                 (ghc-filter-payload rid request (caddr response))
               (message "Bogus result for non-existant request from server: %S" response))))
          (t (message "Bogus line from server: %S" response)))))
    (puthash pid remainder ghc-buffers)))

(defun ghc-filter-payload (rid request payload)
  "Handle the final payload, calling appropriate handlers."
  (let ((cmd (ghc-request-cmd request))
        (filter (ghc-request-filter request))
        (complete (ghc-request-complete request))
        (error (ghc-request-error request)))
    (message "<- %S"
             (list (car payload)
                   rid
                   (cadr payload)))
    (case (car payload)
      (result
       (if filter
           (apply filter (list request (cadr payload)))
         (message "Partial results are not supported by this command %S: %S"
                  cmd payload)))
      (end-result
       (remhash rid ghc-requests)
       (if complete
           (apply complete (list request (cadr payload)))
         (message "End results are not supported byp this command %S: %S"
                  cmd payload)))
      (error-result
       (remhash rid ghc-requests)
       (if error
           (apply error (list request (cadr payload)))
         (message "Error results are not handled by this command: %S\nThe error was: %S"
                  cmd payload)))
      (t
       (message "Bogus result type: %S" payload)))))

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
        (when process
          (delete-process process))
        (make-network-process
         :name "*ghc*"
         :host (or "localhost" (read-from-minibuffer "Host: " "localhost"))
         :service (or 5233
                      (string-to-number
                       (read-from-minibuffer "Port: " "5233")))
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

(defun ghc-pong-complete (request result)
  (ecase (car result)
    (pong
     (let ((start (nth 1 result))
           (end (round (* 1000 (float-time)))))
       (message "Ping reply: %dms" (- end start))))))

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
             :complete 'ghc-load-target-complete
             :error 'ghc-load-target-error)))

(defun ghc-load-target-filter (request result)
  (message "Load filter: %S" result))

(defun ghc-load-target-complete (request result)
  (ecase (car result)
    (load-result
     (ecase (nth 1 result)
       (succeeded (message "OK."))))))

(defun ghc-load-target-error (request result)
  (message "Load error: %s" result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type of command

(defun ghc-type (string)
  "Get the type of the given expression."
  (interactive
   (list
    (read-from-minibuffer "Type of: " (haskell-ident-at-point))))
  (ghc-send (ghc-process)
            (make-ghc-request
             :state nil
             :cmd `(type ,string)
             :complete 'ghc-type-complete
             :error 'ghc-type-error)))

(defun ghc-type-complete (request result)
  (message "Type: %s" (cadr result)))

(defun ghc-type-error (request error)
  (message "Type query error: %s" error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kind of command

(defun ghc-kind (string)
  "Get the kind of the given type expression."
  (interactive (list (read-from-minibuffer "Kind of: ")))
  (ghc-send (ghc-process)
            (make-ghc-request
             :state nil
             :cmd `(kind ,string)
             :complete 'ghc-kind-complete
             :error 'ghc-kind-error)))

(defun ghc-kind-complete (request result)
  (message "Kind: %s" (cadr result)))

(defun ghc-kind-error (request error)
  (message "Kind query error: %s" error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info of command

(defun ghc-info (string)
  "Get the info of the given thing."
  (interactive (list (read-from-minibuffer "Info of: ")))
  (ghc-send (ghc-process)
            (make-ghc-request
             :state nil
             :cmd `(info ,string)
             :complete 'ghc-info-complete
             :error 'ghc-info-error)))

(defun ghc-info-complete (request result)
  (message "%s" (cadr result)))

(defun ghc-info-error (request error)
  (message "Info error: %s" error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eval command

(defun ghc-eval (string)
  "Evaluate an expression and show the result in the REPL."
  (interactive (list (read-from-minibuffer "Eval: ")))
  (ghc-send (ghc-process)
            (make-ghc-request
             :state (current-buffer)
             :cmd `(eval ,string)
             :complete 'ghc-mode-eval-complete
             :filter 'ghc-mode-eval-filter
             :error 'ghc-mode-eval-error)))

(defun ghc-mode-eval-filter (request type)
  "Handler for a completed eval command."
  (ecase (car type)
    (type-result
     (message ":: %s" (cadr type)))
    (eval-import
     (message "Imported, context:\n%s"
              (mapconcat 'identity
                         (cadr type)
                         "\n")))
    (eval-stderr
     (message "Stderr: %s" (cadr type)))
    (eval-stdout
     (message "Stdout: %s" (cadr type)))))

(defun ghc-mode-eval-complete (request result)
  "Handler for a completed eval command."
  (ecase (car result)
    (unit
     (message "Completed."))
    (eval-result
     (message "Eval result: %s" (cadr result)))
    (decl-result
     (message "Declared names: %s"
              (mapconcat 'identity
                         (cadr result)
                         ", ")))))

(defun ghc-mode-eval-error (request error)
  "Handler for a completed eval command."
  (message "Evaluation error: %s" (replace-regexp-in-string "\n" " " error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set

(defun ghc-set (opt)
  "Set some GHC option."
  (interactive (list (read-from-minibuffer "Option: ")))
  (ghc-send (ghc-process)
            (make-ghc-request
             :cmd `(set ,opt)
             :complete 'ghc-set-ok)))

(defun ghc-set-ok (request _)
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
