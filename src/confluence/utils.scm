(define-module (confluence utils)
  #:use-module (srfi srfi-43)
  #:use-module (service logger)
  #:export (vector-parse-block
             parse-text-styles))

(define (vector-parse-block content proc)
  (let ((md ""))
    (vector-for-each
      (lambda (i block) (set! md (string-append md (proc block))))
      content)
    md))

(define (parse-text-styles marks)
  (if (vector? marks)
    (vector->list (vector-map
            (lambda (i mark) (assoc-ref mark "type"))
            marks))
    '()))

