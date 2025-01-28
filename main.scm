(use-modules
  (json)
  (ice-9 match))

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

(format #t "~a\n" (assoc-ref
                    (get-page-atlas confluence-page-json) "type"))
