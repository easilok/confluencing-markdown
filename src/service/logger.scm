(define-module (service logger)
  #:use-module (ice-9 textual-ports)
  #:export (log-msg
             create-logger))

(define log-filepath ".log")

(define log-msg
  (lambda (msg) #f))

;; TODO: make with-file? optional
(define (create-logger with-file?)
  (put-string (open-file log-filepath "w") "")
  (if with-file?
    (set! log-msg
      (lambda (msg)
        (put-string (open-file log-filepath "a") msg)))))

