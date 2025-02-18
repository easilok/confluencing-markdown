(define-module (markdown document)
  #:use-module (ice-9 regex)
  #:export (clean-markdown-newlines))

(define (clean-markdown-newlines content)
  ;; Will only support two newlines to separate different markdown blocks
  (regexp-substitute/global #f "[\n]{3,}" content 'pre "\n\n" 'post))

