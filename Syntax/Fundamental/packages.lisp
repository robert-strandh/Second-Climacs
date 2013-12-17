(cl:in-package #:common-lisp-user)

(defpackage #:climacs-analyzer-fundamental
  (:use #:common-lisp)
  (:shadow fresh-line)
  (:export
   #:paragraphs
   #:lines
   #:buffer-line
   #:buffer
   #:fundamental-analyzer
   #:synchronize
   ;; FIXME: replace this with a generic function that dispatches
   ;; on the type of the analyzer.
   #:update
   ))

