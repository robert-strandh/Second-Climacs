(cl:in-package #:climacs-syntax-common-lisp)

;;; We indent MAKE-INSTANCE in a special way, because we can then
;;; often both save horizontal space and make the form more readable.
;;; The first argument to MAKE-INSTANCE (i.e. the CLASS form) is
;;; indented by 6 columns in case it is not on the same line as the
;;; MAKE-INSTANCE symbol.  Each keyword form is indented by 2
;;; positions compared to the entire form and each value form is
;;; indented by 4 positions.  That way, if there are many long keyword
;;; arguments, it is possible to put the keyword form on one line and
;;; the value form on the next one.  Here is an example:
;;;
;;; (make-instance <class>
;;;   :first-keyword
;;;     (cond ...
;;;           ...
;;;           ...)
;;;   :second-keyword
;;;     (case ...
;;;           ...))

