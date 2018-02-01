(cl:in-package #:common-lisp-user)

(defpackage #:climacs-syntax-common-lisp
  (:use #:common-lisp)
  (:shadow #:package-name)
  (:local-nicknames (#:reader #:sicl-reader))
  (:export
   #:folio
   #:line-count
   #:line-length
   #:line-contents
   #:item
   #:folio-stream
   #:synchronize
   #:common-lisp-view
   #:set-common-lisp-mode
   #:view
   #:analyzer
   #:scavenge
   #:read-forms
   #:parse-result
   #:expression-parse-result
   #:no-expression-parse-result
   #:comment-parse-result
   #:eof-parse-result
   #:error-parse-result
   #:start-line
   #:height
   #:end-line
   #:start-column
   #:end-column
   #:min-column-number
   #:max-column-number
   #:children
   #:expression
   #:token
   #:symbol-token
   #:legal-symbol-token
   #:illegal-symbol-token
   #:non-existing-package-symbol-token
   #:non-existing-symbol-token
   #:existing-symbol-token
   #:package-name
   #:package-marker-1
   #:package-marker-2
   #:name
   #:cache
   #:prefix
   #:suffix
   #:push-to-prefix
   #:pop-from-prefix
   #:push-to-suffix
   #:pop-from-suffix
   #:prefix-to-suffix
   #:suffix-to-prefix
   #:min-column-number
   #:max-column-number
   #:max-line-width))
