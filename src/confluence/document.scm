(define-module (confluence document)
  #:use-module (json)
  #:use-module (util common)
  #:export (confluence-get-page
             get-page-atlas-content))

;; TODO: This will eventually fetch the page from Atlassian API.
;; TODO: Could need proper validation of the file loading
(define (confluence-get-page filepath)
  (get-page-atlas-content (load-json-file filepath)))

;; TODO: json lib has a :ordered prop to keep order of json objects
(define (get-page-atlas-content page)
  (let* ((body (assoc-ref page "body"))
         (atlas (assoc-ref body "atlas_doc_format"))
         (content (assoc-ref atlas "value")))
    (json-string->scm content)))

