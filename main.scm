(use-modules
  (json)
  (ice-9 match))

; (define confluence-page-export "doc-full.json")
(define confluence-page-export "doc-simple.json")

(define (load-json-file path)
  (with-input-from-file
    path
    (lambda() (json->scm))))


(define confluence-page-json 
  (load-json-file confluence-page-export))

(define (get-page-atlas page)
  (let* ((body (assoc-ref confluence-page-json "body"))
         (atlas (assoc-ref body "atlas_doc_format"))
         (content (assoc-ref atlas "value")))
    (json-string->scm content)))

; (display confluence-page-json)

; (format #t "~a\n" (assoc "body" confluence-page-json))
; (display (assoc-ref confluence-page-json "spaceId"))
; (display (car confluence-page-json))

(define (parse-text-block block)
  (assoc-ref block "text"))

(define (parse-bodied-extension-block block)
  (string-append 
    (assoc-ref block "title")
    (map atlas->md (assoc-ref block "content"))))

(define (default-parse-block block)
    (map atlas->md (assoc-ref block "content")))

(define (atlas->md block)
  (let ((type (assoc-ref block "type")))
    (format #t "Evaluating type ~a\n" type)
    (match type
           ("doc" (default-parse-block block))
           (_ (format #f "Unknown '~a' type\n" type)))))

(display (atlas->md (get-page-atlas confluence-page-json)))
; (format #t "~a\n" (assoc-ref
;                     (get-page-atlas confluence-page-json) "type"))
