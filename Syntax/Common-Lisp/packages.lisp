(cl:in-package #:common-lisp-user)

(defpackage #:climacs-syntax-common-lisp
  (:use #:common-lisp)
  (:shadow #:package-name)
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
   #:package-name
   #:package-marker-count
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
