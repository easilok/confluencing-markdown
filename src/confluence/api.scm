(define-module (confluence api)
  #:use-module (confluence document)
  #:use-module (gnutls)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (web client)
  #:export (fetch-confluence-page))

(define default-query-params "body-format=atlas_doc_format")

(define* (fetch-confluence-page #:key username password base-url page-id (query-params default-query-params))
         (let* ((credentials (base64-encode (string-append username ":" password)))
                (headers `((Authorization . ,(string-append "Basic " credentials))
                          (Content-Type . "application/json")))
                (url (format #f "~a/~a?~a" base-url page-id query-params)))
           (let-values (((response body)
                         (http-request
                           url
                           ; #:decode-body? #t
                           #:headers headers)))
             ;; TODO: check for error status and exit applicationin error 
             (get-page-atlas-content (json-string->scm (utf8->string body) #:ordered #t)))))
