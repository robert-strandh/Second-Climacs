(cl:in-package #:climacs-syntax-common-lisp)

(defparameter *backquote-allowed-p* t)

(defparameter *backquote-in-subforms-allowed-p* t)

(defparameter *backquote-depth* 0)

(defvar *consing-dot* '#:|.|)

(defparameter *consing-dot-allowed-p* nil)

(defparameter *preserve-whitespace* nil)
