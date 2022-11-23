(cl:in-package #:climacs-syntax-common-lisp)

(defun set-common-lisp-mode (view)
  (change-class (base:analyzer view) 'cache)
  (change-class view 'common-lisp-view))
