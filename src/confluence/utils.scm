(define-module (confluence utils)
  #:use-module (service logger)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-43)
  #:export (vector-parse-block
             parse-text-styles
             clean-confluence-newlines
             convert-confluence-panel-type))

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

(define (clean-confluence-newlines content)
  (string-trim-both content #\newline))

(define (convert-confluence-panel-type type)
  ; Convert confluence panel type into GitHub markdown panel types
  (match type
         (t (string-upcase type))))
