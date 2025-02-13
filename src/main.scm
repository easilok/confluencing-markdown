(use-modules
  (util common)
  (service logger)
  (confluence document)
  (confluence parser))

(define confluence-page-export "doc-full.json")
; define confluence-page-export "../doc-simple.json")
; Set this to true for file logging
(define logger? #f)

(create-logger logger?)
(define markdown (atlas->md (confluence-get-page confluence-page-export)))
(log-msg (format #f "Unparsed types: ~a\n" atlas-unparsed-block-types))

(format #t "~a\n" markdown)
