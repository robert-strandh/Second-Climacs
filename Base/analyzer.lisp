(cl:in-package #:second-climacs-base)

;;; An ANALYZER is an object that interprets the contents of a buffer
;;; in a particular way.
;;;
;;; The analyzer typically imposes restrictions on the kind of buffer
;;; that it can handle, because it will typically use a specific
;;; protocol to access the data in the buffer.
;;;
;;; Several analyzers can share a single buffer object.  This
;;; situation occurs when it is desirable to analyze the same contents
;;; in more than one way for different purposes.
;;;
;;; Analyzers may cascade in that an analyzer may contain another
;;; analyzer.  This situation would occur when some buffer contents
;;; can be analyzed in more than one way, but the different ways have
;;; a large common component that could be factored out into a
;;; separate, preliminary, analyzer.

(defclass analyzer ()
  ((%buffer :initarg :buffer :reader buffer)))
