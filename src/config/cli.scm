(define-module (config cli)
    #:use-module (ice-9 getopt-long)
    #:export (load-options
               save-markdown))


(define option-spec
  '((output (single-char #\o) (value #t))
    (log-file (value #t))
    (logger (single-char #\l) (value #f))))

(define (load-options)
  (getopt-long (command-line) option-spec))

(define (save-markdown markdown output-file)
  (if (and output-file (not (string-null? output-file)))
    (with-output-to-file
      output-file
      (lambda ()
        (format #t "~a\n" markdown)))
    (format #t "~a\n" markdown)))
