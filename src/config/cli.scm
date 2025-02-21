(define-module (config cli)
    #:use-module (json)
    #:use-module (ice-9 getopt-long)
    #:export (load-options
               load-config
            valid-cli-argument
               save-markdown))


(define option-spec
  '((output (single-char #\o) (value #t))
    (logger (single-char #\l) (value #f))
    (log-file (value #t))
    (config (single-char #\c) (value #t))
    (base-url (single-char #\b) (value #t))
    (page-id (single-char #\p) (value #t))))

(define (load-options)
  (getopt-long (command-line) option-spec))

(define (load-config file-path)
  (if (access? file-path R_OK)
    (with-input-from-file
      file-path
      (lambda ()
        (json->scm)))
    #f))


(define (save-markdown markdown output-file)
  (if (and output-file (not (string-null? output-file)))
    (with-output-to-file
      output-file
      (lambda ()
        (format #t "~a\n" markdown)))
    (format #t "~a\n" markdown)))

(define (valid-cli-argument arg)
  (and (string? arg) (not (string-null? arg))))
