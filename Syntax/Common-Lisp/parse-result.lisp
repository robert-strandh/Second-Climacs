(cl:in-package #:climacs-syntax-common-lisp)

(defclass parse-result ()
  ((%start-line :initarg :start-line :accessor start-line)
   ;; This slot contains the difference between the start line and the
   ;; end line.  A value of 0 indicates that the parse result starts
   ;; and ends in the same line.
   (%height :initarg :height :reader height)
   (%start-column :initarg :start-column :accessor start-column)
   (%end-column :initarg :end-column :accessor end-column)
   ;; This slot contains TRUE if and only if the START-LINE slot is
   ;; relative to some other line.
   (%relative-p :initarg :relative-p :accessor relative-p)
   (%children :initarg :children :accessor children)))

(defclass expression-parse-result (parse-result)
  ((%expression :initarg :expression :accessor expression)))

(defclass no-expression-parse-result (parse-result)
  ())

(defclass error-parse-result (parse-result)
  ())

(defclass eof-parse-result (parse-result)
  ())

(defgeneric relative-to-absolute (parse-result offset)
  (:method ((p parse-result) offset)
    (assert (relative-p p))
    (incf (start-line p) offset)
    (setf (relative-p p) nil)))

(defgeneric absolute-to-relative (parse-result offset)
  (:method ((p parse-result) offset)
    (assert (not (relative-p p)))
    (decf (start-line p) offset)
    (setf (relative-p p) t)))

;;; RELATIVE-PARSE-RESULTS is a list of parse results where the start
;;; line of the first element is relative to OFFSET, and the start
;;; line of each of the other elements is relative to the start line
;;; of the preceding element.  Return a list of absolute parse
;;; results.
(defun make-absolute (relative-parse-results offset)
  (loop with base = offset
	for parse-result in relative-parse-results
	do (relative-to-absolute parse-result base)
	   (setf base (start-line parse-result))))

;;; ABSOLUTE-PARSE-RESULTS is a list of absolute parse results.
;;; Return a list of parse results where the start line of the first
;;; element is absolute to OFFSET, and the start line of each of the
;;; other elements is relative to the start line of the preceding
;;; element.
(defun make-relative (absolute-parse-results offset)
  (loop with base = offset
	for parse-result in absolute-parse-results
	for start-line = (start-line parse-result)
	do (absolute-to-relative parse-result base)
	   (setf base start-line)))

(defgeneric end-line (parse-result)
  (:method ((p parse-result))
    (assert (not (relative-p p)))
    (+ (start-line p) (height p))))
