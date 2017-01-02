(cl:in-package #:climacs-syntax-common-lisp)

(defun set-common-lisp-mode (view)
  (change-class (climacs2-base:analyzer view) 'analyzer)
  (change-class view 'common-lisp-view))
