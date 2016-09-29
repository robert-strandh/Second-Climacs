(cl:in-package #:climacs-syntax-common-lisp)

(defclass parse-result ()
  ((%start-line :initarg :start-line :accessor start-line)
   (%end-line :initarg :end-line :accessor end-line)
   (%start-column :initarg :start-column :accessor start-column)
   (%end-column :initarg :end-column :accessor end-column)
   (%children :initarg :children :accessor children)
   (%expression :initarg :expression :accessor expression)))

