(define-module (confluence utils)
  #:use-module (service logger)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (vector-parse-block
             parse-text-styles
             clean-confluence-newlines
             convert-confluence-panel-type
             complete-table-layout))

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

(define (header-column? column)
  (or
    (string-null? column)
    (string-prefix? "**" (string-trim-both column))))

(define* (build-table-header column-count #:optional (empty-header? #f))
  (let ((columns (make-list column-count " --- ")))
    (if empty-header?
      (format #f "| ~a |\n| ~a |"
              (string-join (make-list column-count "") "|")
              (string-join columns "|"))
      (format #f "| ~a |" (string-join columns "|")))))

(define (complete-table-layout table)
  (let* ((lines (string-split table #\newline))
         (columns (string-split (string-trim-both (car lines) #\|) #\|)))
    (if (every header-column? columns)
      (format #f "~a\n~a\n~a\n"
              (car lines)
              (build-table-header (length columns))
              (string-join (cdr lines) "\n"))
      (format #f "~a\n~a\n"
              (build-table-header (length columns) #t)
              table))))
