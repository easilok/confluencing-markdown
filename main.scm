(use-modules
  (json)
  (ice-9 i18n)
  (ice-9 match)
  (ice-9 string-fun)
  (ice-9 textual-ports)
  (srfi srfi-19)
  (srfi srfi-43))

(define confluence-page-export "doc-full.json")
; define confluence-page-export "doc-simple.json")
(define unparsed-block-types '())
; Set this to true for file logging
(define logger? #f)

(define (create-logger)
  (put-string (open-file ".log" "w") "")
  (if logger?
    (lambda (msg)
      (put-string (open-file ".log" "a") msg))
    (lambda (msg) #f)))

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
    (log-msg (format #f "Found text '~a' with style ~a\n" text style))
    ; TODO: marks is an array of types
    (match style
           ("strong" (format #f "**~a**" text))
           ("code" (format #f "`~a`" text))
           (_ text))))

(define (parse-date-block block)
  (let* ((epoch (recursive-assoc-ref block '("attrs" "timestamp")))
         (date (string-epoch->date epoch)))
    (log-msg (format #f "Found date '~a'\n" epoch))
    (format #f "~a" (date->string date "~b ~e, ~Y"))))

(define (parse-status-block block)
  (let ((text (recursive-assoc-ref block '("attrs" "text"))))
    (log-msg (format #f "Found status '~a'\n" text))
    (format #f "`~a`" text)))

(define (parse-emoji-block block)
  (let ((text (recursive-assoc-ref block '("attrs" "text"))))
    (log-msg (format #f "Found emoji '~a'\n" text))
    (format #f "~a" text)))

(define (parse-mention-block block)
  (let ((text (recursive-assoc-ref block '("attrs" "text"))))
    (log-msg (format #f "Found mention '~a'\n" text))
    (format #f "*~a*" text)))

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
            (string-trim-both
              (atlas->md (assoc-ref block "content"))
              #\newline)
              "\n" "<br/>")))

(define (parse-bullet-list-block block)
  (format #f "\n\n~a\n" (atlas->md (assoc-ref block "content"))))

(define (parse-list-item-block block)
  (format #f "- ~a\n" (atlas->md (assoc-ref block "content"))))

(define (parse-heading-block block)
  (let ((level (recursive-assoc-ref block '("attrs" "level"))))
    (format #f "\n\n~a ~a\n\n"
            (string-pad "" level #\#)
            (atlas->md (assoc-ref block "content")))))

(define (parse-panel-block block)
  (let ((type (recursive-assoc-ref block '("attrs" "panelType"))))
    (format #f "\n\n> [!~a]\n> ~a\n\n"
            ; Convert confluence panel type into GitHub markdown panel types
            (match type
                   (t (string-upcase type)))
            (atlas->md (assoc-ref block "content")))))

(define (parse-expand-block block)
  (let ((title (recursive-assoc-ref block '("attrs" "title"))))
    (format #f "\n\n<details>\n\t<summary>~a</summary>\n\n~a</details>\n\n"
            title
            (atlas->md (assoc-ref block "content")))))

(define (parse-media-block block)
  (let ((alt (recursive-assoc-ref block '("attrs" "alt"))))
    (format #f "\n\n![~a](~a)\n\n"
            alt alt)))

(define (default-parse-block block)
    (atlas->md (assoc-ref block "content")))

(define (atlas->md block)
  (if (vector? block)
    (vector-parse-block block)
    (let ( (type (assoc-ref block "type")))
      ; (log-msg (format #f "Evaluating type ~a\n" type))
      (match type
             ("bodiedExtension" (parse-bodied-extension-block block))
             ("date" (parse-date-block block))
             ("text" (parse-text-block block))
             ("status" (parse-status-block block))
             ("emoji" (parse-emoji-block block))
             ("mention" (parse-mention-block block))
             ("table" (parse-table-block block))
             ("tableRow" (parse-table-row-block block))
             ("tableHeader" (parse-table-header-block block))
             ("tableCell" (parse-table-cell-block block))
             ("heading" (parse-heading-block block))
             ("bulletList" (parse-bullet-list-block block))
             ("listItem" (parse-list-item-block block))
             ("panel" (parse-panel-block block))
             ("expand" (parse-expand-block block))
             ("mediaSingle" (default-parse-block block))
             ("media" (parse-media-block block))
             ; block types with passtrough parsing
             ("doc" (default-parse-block block))
             ("paragraph" (default-parse-block block))
             (#f "")
             (t 
               (set! unparsed-block-types (append unparsed-block-types `(,t)))
               "")
               ; (default-parse-block block))
             ; ("paragraph" (parse-paragraph-block block))
             (_ "")))))

(define log-msg (create-logger))
(define markdown (atlas->md (get-page-atlas confluence-page-json)))
(log-msg (format #f "Unparsed types: ~a\n" unparsed-block-types))

(format #t "~a\n" markdown)
