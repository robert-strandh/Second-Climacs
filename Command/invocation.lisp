(cl:in-package #:climacs-command)

(defclass invocation ()
  ())

(defclass pass-numeric-argument-as-argument (invocation)
  ())

(defclass repeat-according-to-numeric-argument (invocation)
  ())

(defclass pass-gesture-as-argument (invocation)
  ())

(defgeneric invoke (command invocation gesture numeric-arg numeric-arg-p))

(defmethod invoke (command
		   (invocation pass-numeric-argument-as-argument)
		   gesture
		   numeric-arg
		   numeric-arg-p)
  (declare (ignore gesture numeric-arg-p))
  (invoke-command command (list numeric-arg)))

(defmethod invoke (command
		   (invocation repeat-according-to-numeric-argument)
		   gesture
		   numeric-arg
		   numeric-arg-p)
  (declare (ignore gesture numeric-arg-p))
  (loop repeat numeric-arg
	do (invoke-command command '())))

(defmethod invoke (command
		   (invocation pass-gesture-as-argument)
		   gesture
		   numeric-arg
		   numeric-arg-p)
  (declare (ignore numeric-arg numeric-arg-p))
  (invoke-command command (list gesture)))
