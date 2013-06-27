(defvar ghc-request-number
  0
  "A unique request number counter.")

(defstruct ghc-request
  "A request handler."
  state cmd filter complete)

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
                 (let ((payload (caddr response)))
                   (case (car payload)
                     ('result (apply (ghc-request-filter request)
                                       (cdadr payload)))
                     ('end-result (apply (ghc-request-complete request)
                                           (cdadr payload))
                                  (remhash rid ghc-requests))))
               (message "Bogus result for non-existant request from server: %S" response))))
          (t (message "Bogus line from server: %S" response)))))
    (puthash pid remainder ghc-buffers)))

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

(ghc-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ghc-send (ghc-process)
          (make-ghc-request
           :state nil
           :cmd `(ping ,(round (* 1000 (float-time))))
           :complete 'ghc-pong-complete))

(defun ghc-pong-complete (start)
  (let ((end (round (* 1000 (float-time)))))
    (message "Ping reply: %dms" (- end start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ghc-send (ghc-process)
          (make-ghc-request
           :state nil
           :cmd '(eval "\"Hey!\"")
           :complete 'ghc-eval-complete))

(defun ghc-eval-complete (result)
  (message "→ %s" result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ghc-send (ghc-process)
          (make-ghc-request
           :state nil
           :cmd '(load-target "src/Main.hs")
           :filter 'ghc-load-target-filter
           :complete 'ghc-load-target-complete))

(ghc-send (ghc-process)
          (make-ghc-request
           :state nil
           :cmd '(eval "foo")
           :filter 'ghc-eval-filter
           :complete 'ghc-eval-complete))

(defun ghc-load-target-filter (result)
  (message "Load filter: %S" result))

(defun ghc-load-target-complete (result)
  (message "Load: %S" result))
