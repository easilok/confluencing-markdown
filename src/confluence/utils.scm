(define-module (confluence utils)
  #:use-module (srfi srfi-43)
  #:export (vector-parse-block))

(define (vector-parse-block content proc)
  (let ((md ""))
    (vector-for-each
      (lambda (i block) (set! md (string-append md (proc block))))
      content)
    md))

