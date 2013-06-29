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
    (process-send-string p
                         (format "%S\n" `(request ,rid ,(ghc-request-cmd request))))))

(defun ghc-sentinel (p sig)
  "Handles connection events."
  (message "%S" sig))

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
  (let ((cmd (ghc-request-cmd request))
        (filter (ghc-request-filter request))
        (complete (ghc-request-complete request))
        (error (ghc-request-error request)))
    (case (car payload)
      ('result (if filter
                   (apply filter (cdadr payload))
                 (message "Partial results are not supported by this command %S: %S"
                          cmd payload)))
      ('end-result (if complete
                       (apply complete (cdadr payload))
                     (message "End results are not supported byp this command %S: %S"
                              cmd payload))
                   (remhash rid ghc-requests))
      ('error-result (if error
                         (apply error (cdr payload))
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

(defun ghc-pong-complete (start)
  (let ((end (round (* 1000 (float-time)))))
    (message "Ping reply: %dms" (- end start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eval command

(defun ghc-eval (string)
  "Evaluate an expression."
  (interactive (list (read-from-minibuffer "Eval: ")))
  (ghc-send (ghc-process)
            (make-ghc-request
             :state nil
             :cmd `(eval ,string)
             :complete 'ghc-eval-complete)))

(defun ghc-eval-complete (result)
  (message "→ %s" result))

(defun ghc-eval-error (error)
  (message "Error: %s" error))

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

(defun ghc-load-target-filter (result)
  (message "Load filter: %S" result))

(defun ghc-load-target-complete (result)
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

(defun ghc-type-complete (result)
  (message ":: %s" result))

(defun ghc-type-error (error)
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

(defun ghc-kind-complete (result)
  (message ":: %s" result))

(defun ghc-kind-error (error)
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

(defun ghc-info-complete (result)
  (message "%s" result))

(defun ghc-info-error (error)
  (message "Error: %s" error))
