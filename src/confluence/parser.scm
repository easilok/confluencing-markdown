(define-module (confluence parser)
  #:use-module (confluence utils)
  #:use-module (service logger)
  #:use-module (util common)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-19)
  #:export (atlas->md
             atlas-unparsed-block-types))


(define atlas-unparsed-block-types '())

(define (parse-text-block block)
  (let* ((text (assoc-ref block "text"))
        (marks (assoc-ref block "marks"))
        (styles (parse-text-styles marks)))
    (log-msg (format #f "Found text '~a' with marks '~a' resultint in styles '~a'\n" text marks styles))
    ; TODO: marks is an array of types
    (for-each
      (lambda (style)
        (set! text
          (match style
           ("strong" (format #f "**~a**" text))
           ("code" (format #f "`~a`" text))
           (_ text))))
      styles)
    text))

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
  (format #f "\n\n~a\n" (atlas->md (assoc-ref block "content"))))

(define (parse-bodied-extension-block block)
    (let ((ret "")
          (attrs (assoc-ref block "attrs"))
          (title (recursive-assoc-ref block '("attrs" "parameters" "macroMetadata" "title")))
          (content (assoc-ref block "content")))
      (if title
        (set! ret title))
      (format #f "## ~a\n\n~a" ret (atlas->md content))))

(define (parse-table-block block)
  (let ((table (atlas->md (assoc-ref block "content"))))
    (format #f "\n\n~a\n" (complete-table-layout table))))

(define (parse-table-row-block block)
  (format #f "|~a\n" (atlas->md (assoc-ref block "content"))))

(define (parse-table-header-block block)
  (let ((th (clean-confluence-newlines
              (atlas->md (assoc-ref block "content")))))
      (if (string-contains th "**")
        (format #f " ~a |" th)
        (format #f " **~a** |" th))))

(define (parse-table-cell-block block)
  (format #f " ~a |"
          (string-replace-substring
            (clean-confluence-newlines
              (atlas->md (assoc-ref block "content")))
            "\n" "<br/>")))

(define (parse-bullet-list-block block)
  (format #f "\n\n~a\n" (atlas->md (assoc-ref block "content"))))

(define (parse-list-item-block block)
  (format #f "- ~a\n" (clean-confluence-newlines
                        (atlas->md (assoc-ref block "content")))))

(define (parse-heading-block block)
  (let ((level (recursive-assoc-ref block '("attrs" "level"))))
    (format #f "\n\n~a ~a\n\n"
            (string-pad "" level #\#)
            (atlas->md (assoc-ref block "content")))))

(define (parse-panel-block block)
  (let ((type (recursive-assoc-ref block '("attrs" "panelType"))))
    (log-msg (format #f "Found panel with type '~a'\n" type))
    (format #f "\n\n> [!~a]\n> ~a\n\n"
            (convert-confluence-panel-type type)
            (clean-confluence-newlines (atlas->md (assoc-ref block "content"))))))

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
    (vector-parse-block block atlas->md)
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
             ("paragraph" (parse-paragraph-block block))
             (#f "")
             (t 
               (set! atlas-unparsed-block-types (append atlas-unparsed-block-types `(,t)))
               "")
               ; (default-parse-block block))
             (_ "")))))

