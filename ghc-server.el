(make-network-process :name "*ghc*"
                      :host "localhost"
                      :service "5233"
                      :nowait t
                      :sentinel 'ghc-sentinel
                      :filter 'ghc-filter)

(defvar ghc-request-number 0)

(defun ghc-send (p cmd)
  (setq ghc-request-number (1+ ghc-request-number))
  (process-send-string p (format "%S\n" `(request ,ghc-request-number ,cmd))))

(defun ghc-sentinel (p sig)
  (message "%S" sig))

(defun ghc-filter (p data)
  (let ((response (read data)))
    (message "%S" response)))
