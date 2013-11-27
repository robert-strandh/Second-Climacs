(cl:in-package #:common-lisp-user)

(defpackage #:climacs-show-fundamental
  (:use #:common-lisp)
  (:import-from #:climacs-analyzer-fundamental
		#:fundamental-analyzer
		#:paragraphs
		#:lines
		#:buffer-line)
  (:export
   #:fundamental-show
   ))
