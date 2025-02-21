#!/usr/bin/env -S guile -e main -L src/ -s
!#

(use-modules
  (config cli)
  (confluence api)
  (confluence document)
  (confluence parser)
  (markdown document)
  (service logger)
  (util common)
  (ice-9 getopt-long))

(define (main args)
    (let* ((user-options (load-options))
            (output-file (option-ref user-options 'output #f))
            (rest-args (option-ref user-options '() #f))
            (logger? (option-ref user-options 'logger #f))
            (log-file (option-ref user-options 'log-file #f))
            (config (option-ref user-options 'config #f))
            (base-url (option-ref user-options 'base-url #f))
            (input-file #f)
            (username #f)
            (password #f)
            (page-id (option-ref user-options 'page-id #f)))

      (if config (set! config (load-config config)))
      (set! username (recursive-assoc-ref config '("confluence" "username")))
      (set! password (recursive-assoc-ref config '("confluence" "password")))
      (set! page-id (or page-id (recursive-assoc-ref config '("confluence" "page"))))
      (set! base-url (or base-url (recursive-assoc-ref config '("confluence" "base-url"))))
      (if (not (null? rest-args))
        (set! input-file (car rest-args)))

      (format #t "Config: ~a\n" config)
      (format #t "Username: ~a\n" username)
      (format #t "BaseUrl: ~a\n" base-url)
      (format #t "PageId: ~a\n" page-id)

      (if (and
            (not (valid-cli-argument input-file))
            (not (and
                   (valid-cli-argument username)
                   (valid-cli-argument password)
                   (valid-cli-argument base-url)
                   (valid-cli-argument page-id))))
        (begin
          (format #t "Must provide the input file to parse or details to fetch page from confluence!\n")
          (exit 1)))

      (if (and logger? (not log-file) (not output-file))
        (begin
          (format #t "Either log-file or output-file must be set to enable logging!\n")
          (exit 1)))

      (format #t "Starting Confluence parser with:\n")
      (if input-file
        (format #t "\t- Input: ~a\n" input-file)
        (format #t "\t- URL: ~a\n\t- Page: ~a\n" base-url page-id))

      (format #t "\t- Output: ~a\n" output-file)

      (if (or logger? log-file)
        (begin
          (format #t "\t- Log-file: ~a\n" log-file)
          (create-logger log-file)))

      (let ((document #f)
            (markdown #f))
        (if (valid-cli-argument input-file)
          (set! document (confluence-get-page input-file))
          (set! document (fetch-confluence-page
                           #:username username
                           #:password password
                           #:base-url base-url
                           #:page-id page-id)))
        (set! markdown (atlas->md document))

        (log-msg (format #f "Unparsed types: ~a\n" atlas-unparsed-block-types))
        (save-markdown
          (clean-markdown-newlines markdown)
          output-file))))
