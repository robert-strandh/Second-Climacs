(cl:in-package #:climacs-syntax-common-lisp)

(defclass parse-result ()
  ((%start-line :initarg :start-line :accessor start-line)
   ;; This slot contains the difference between the start line and the
   ;; end line.  A value of 0 indicates that the parse result starts
   ;; and ends in the same line.
   (%height :initarg :height :reader height)
   (%start-column :initarg :start-column :accessor start-column)
   (%end-column :initarg :end-column :accessor end-column)
   ;; This slot contains the column number of the leftmost known
   ;; non-whitespace character of the parse result.  It may not be
   ;; entirely correct if a reader macro reads character by character
   ;; and such characters happen to be outside the part that is
   ;; returned by a call to READ.  But we only use this information
   ;; for highlighting, and selection.  Not for drawing.
   (%min-column-number :reader min-column-number)
   ;; This slot contains the column number of the leftmost known
   ;; non-whitespace character of the parse result.  It may not be
   ;; entirely correct for the same reason as the preceding slot.
   (%max-column-number :reader max-column-number)
   ;; This slot contains TRUE if and only if the START-LINE slot is
   ;; relative to some other line.
   (%relative-p :initarg :relative-p :accessor relative-p)
   (%children :initarg :children :accessor children)))

(defmethod print-object ((object parse-result) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~d,~d -> ~d,~d) rel: ~s"
            (start-line object)
            (start-column object)
            (end-line object)
            (end-column object)
            (relative-p object))))

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
;;; of the preceding element.  Modify the parse results in the list so
;;; that they are absolute.  Return the original list, now containing
;;; the modified parse results.
(defun make-absolute (relative-parse-results offset)
  (loop with base = offset
	for parse-result in relative-parse-results
	do (relative-to-absolute parse-result base)
	   (setf base (start-line parse-result)))
  relative-parse-results)

;;; ABSOLUTE-PARSE-RESULTS is a list of absolute parse results.
;;; Modify the parse results in the list so that the start line of the
;;; first element is absolute to OFFSET, and the start line of each of
;;; the other elements is relative to the start line of the preceding
;;; element.  Return the original list, now containing the modified
;;; parse results.
(defun make-relative (absolute-parse-results offset)
  (loop with base = offset
	for parse-result in absolute-parse-results
	for start-line = (start-line parse-result)
	do (absolute-to-relative parse-result base)
	   (setf base start-line))
  absolute-parse-results)

(defgeneric end-line (parse-result)
  (:method ((p parse-result))
    (assert (not (relative-p p)))
    (+ (start-line p) (height p))))
