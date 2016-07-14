(cl:in-package #:climacs-command)

(defclass invocation ()
  ())

(defclass pass-numeric-argument-as-argument (invocation)
  ())

(defclass repeat-according-to-numeric-argument (invocation)
  ())

(defclass pass-gesture-as-argument (invocation)
  ())
