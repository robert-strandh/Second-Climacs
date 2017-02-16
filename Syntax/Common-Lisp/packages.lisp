(cl:in-package #:common-lisp-user)

(defpackage #:climacs-syntax-common-lisp
  (:use #:common-lisp)
  (:export
   #:folio
   #:line-count
   #:line-length
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
   #:prefix
   #:suffix
   #:min-column-number
   #:max-column-number
   #:max-line-width-list))
