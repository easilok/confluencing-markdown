(define-module (util common)
  #:use-module (json)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:export (load-json-file
             recursive-assoc-ref
             string-epoch->date))

(define (load-json-file path)
  (with-input-from-file
    path
    (lambda() (json->scm))))

(define (recursive-assoc-ref a keys)
  (match keys
         ((k rest ...)
          (let ((value (assoc-ref a k)))
            (if value
              (recursive-assoc-ref value rest)
              #f)))
         (() a)))

(define (string-epoch->date str)
  (let* ((epoch (string->number str))
         ; epoch comes in milliseconds and `make-time` accepts seconds
         (t (make-time time-utc 0 (/ epoch 1000))))
    (time-utc->date t)))
