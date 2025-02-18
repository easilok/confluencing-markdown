#!/usr/bin/env -S guile -e main -L src/ -s
!#

(use-modules
  (config cli)
  (confluence document)
  (confluence parser)
  (markdown document)
  (service logger)
  (util common)
  (ice-9 getopt-long))

(define confluence-page-export "doc-full.json")
; (define confluence-page-export "doc-simple.json")
; Set this to true for file logging

(define (main args)
    (let* ((user-options (load-options))
            (output-file (option-ref user-options 'output #f))
            (input-file (option-ref user-options '() #f))
            (logger? (option-ref user-options 'logger #f))
            (log-file (option-ref user-options 'log-file ".log")))

      (if (or (not input-file) (null? input-file))
        (begin
          (format #t "Must provide the input file to parse!\n")
          (exit 1)))

      (format #t "Starting Confluence parser with:\n")
      (format #t "\t- Input: ~a\n\t- Output: ~a\n" input-file output-file)
      (if (or logger? log-file) (format #t "\t- Log-file: ~a\n" log-file))

      (create-logger logger?)

      (let ((markdown (atlas->md (confluence-get-page (car input-file)))))
        (log-msg (format #f "Unparsed types: ~a\n" atlas-unparsed-block-types))
        (save-markdown
          (clean-markdown-newlines markdown)
          output-file))))
