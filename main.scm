(use-modules
  (json)
  (ice-9 match)
  (ice-9 string-fun)
  (srfi srfi-19)
  (srfi srfi-43))

(define confluence-page-export "doc-full.json")
; define confluence-page-export "doc-simple.json")

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

(define (vector-parse-block content)
  (let ((md ""))
    (vector-for-each
      (lambda (i block) (set! md (string-append md (atlas->md block))))
      content)
    md))

(define confluence-page-json 
  (load-json-file confluence-page-export))

(define (get-page-atlas page)
  (let* ((body (assoc-ref confluence-page-json "body"))
         (atlas (assoc-ref body "atlas_doc_format"))
         (content (assoc-ref atlas "value")))
    (json-string->scm content)))

(define (parse-text-block block)
  (let ((text (assoc-ref block "text"))
        (style (recursive-assoc-ref block '("marks" "type"))))
    (format #t "Found text '~a' with style ~a\n" text style)
    ; TODO: marks is an array of types
    (match style
           ("strong" (format #f "**~a**" text))
           ("code" (format #f "`~a`" text))
           (_ text))))

(define (parse-date-block block)
  (let* ((epoch (recursive-assoc-ref block '("attrs" "timestamp")))
         (date (string-epoch->date epoch)))
    (format #t "Found date '~a'\n" epoch)
    (format #f "~a" (date->string date "~b ~e, ~Y"))))

(define (parse-status-block block)
  (let ((text (recursive-assoc-ref block '("attrs" "text"))))
    (format #t "Found status '~a'\n" text)
    (format #f "`~a`" text)))

(define (parse-emoji-block block)
  (let ((text (recursive-assoc-ref block '("attrs" "text"))))
    (format #t "Found emoji '~a'\n" text)
    (format #f "~a" text)))

(define (parse-paragraph-block block)
    "\n\n")

(define (parse-bodied-extension-block-legacy block)
    (let ((ret "")
          (attrs (assoc-ref block "attrs"))
          (content (assoc-ref block "content")))
        (if attrs 
          (let ((parameters (assoc-ref attrs "parameters")))
            (if parameters 
              (let ((metadata (assoc-ref parameters "macroMetadata")))
                (if metadata
                  (let ((title (assoc-ref metadata "title")))
                    (if title
                      (set! ret title))))))))
        (format #f "~a\n\n~a" ret (atlas->md content))))

(define (parse-bodied-extension-block block)
    (let ((ret "")
          (attrs (assoc-ref block "attrs"))
          (title (recursive-assoc-ref block '("attrs" "parameters" "macroMetadata" "title")))
          (content (assoc-ref block "content")))
      (if title
        (set! ret title))
      (format #f "~a\n\n~a" ret (atlas->md content))))

(define (parse-table-block block)
  (format #f "\n\n~a\n" (atlas->md (assoc-ref block "content"))))

(define (parse-table-row-block block)
  (format #f "|~a\n" (atlas->md (assoc-ref block "content"))))

(define (parse-table-header-block block)
  (let ((th (atlas->md (assoc-ref block "content"))))
    (if (string-contains th "**")
      (format #f " ~a |" th))
      (format #f " **~a** |" th)))

(define (parse-table-cell-block block)
  (format #f " ~a |"
          (string-replace-substring
            (string-replace-substring
              (atlas->md (assoc-ref block "content"))
              "\n" "")
            "-" "")))

(define (parse-bullet-list-block block)
  (format #f "\n\n~a\n" (atlas->md (assoc-ref block "content"))))

(define (parse-list-item-block block)
  (format #f "- ~a\n" (atlas->md (assoc-ref block "content"))))

(define (parse-heading-block block)
  (let ((level (recursive-assoc-ref block '("attrs" "level"))))
    (format #f "\n\n~a ~a\n\n" 
            (string-pad "" level #\#)
            (atlas->md (assoc-ref block "content")))))

(define (default-parse-block block)
    (atlas->md (assoc-ref block "content")))

(define (atlas->md block)
  (if (vector? block)
    (vector-parse-block block)
    (let ( (type (assoc-ref block "type")))
      ; (format #t "Evaluating type ~a\n" type)
      (match type
             ("bodiedExtension" (parse-bodied-extension-block block))
             ("date" (parse-date-block block))
             ("text" (parse-text-block block))
             ("status" (parse-status-block block))
             ("emoji" (parse-emoji-block block))
             ("table" (parse-table-block block))
             ("tableRow" (parse-table-row-block block))
             ("tableHeader" (parse-table-header-block block))
             ("tableCell" (parse-table-cell-block block))
             ("heading" (parse-heading-block block))
             ("bulletList" (parse-bullet-list-block block))
             ("listItem" (parse-list-item-block block))
             ; block types with passtrough parsing
             ("doc" (default-parse-block block))
             ("paragraph" (default-parse-block block))
             (#f "")
             (t 
               (format #t "Unparsed type '~a'\n" t)
               "")
               ; (default-parse-block block))
             ; ("paragraph" (parse-paragraph-block block))
             (_ "")))))

(define markdown (atlas->md (get-page-atlas confluence-page-json)))
(format #t "Content:\n\n~a" markdown)
