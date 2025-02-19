(define-module (service logger)
  #:declarative? #f
  #:use-module (ice-9 textual-ports)
  #:export (log-msg
             create-logger))

(define log-msg
  (lambda (msg) #f))

(define* (create-logger #:optional (log-file #f))
         (if log-file
           (begin
             (put-string (open-file log-file "w") "")
             (let ((file (open-file log-file "a")))
               (set! log-msg
                 (lambda (msg)
                   (put-string file msg)))))
           (set! log-msg
             (lambda (msg)
               (format #t "~a\n" msg)))))


