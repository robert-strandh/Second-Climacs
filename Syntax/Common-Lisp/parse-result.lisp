(cl:in-package #:climacs-syntax-common-lisp)

(defclass basic-parse-result ()
  (;; This slot contains information about the start line of the parse
   ;; result.  Simple applications might always store the absolute
   ;; line number of the first line of the parse result in this slot.
   ;; Other applications might store a line number relative to some
   ;; other parse result.
   (%start-line :initarg :start-line :accessor start-line)
   ;; This slot contains the difference between the start line and the
   ;; end line.  A value of 0 indicates that the parse result starts
   ;; and ends in the same line.
   (%height :initarg :height :reader height)
   ;; This slot contains the absolute column of the first character in
   ;; this parse result.  A value of 0 indicates that this parse
   ;; result starts in the leftmost position in the source code.
   (%start-column :initarg :start-column :accessor start-column)
   ;; This slot contains the absolute column of the last character of
   ;; the parse result.  The value of this slot can never be 0.  If
   ;; the last character of the parse result is the leftmost character
   ;; in a line, then this slot contains the value 1.
   (%end-column :initarg :end-column :accessor end-column)))

(defclass parse-result (basic-parse-result)
  (;; This slot contains the column number of the leftmost known
   ;; non-whitespace character of the parse result.  It may not be
   ;; entirely correct if a reader macro reads character by character
   ;; and such characters happen to be outside the part that is
   ;; returned by a call to READ.  But we only use this information
   ;; for highlighting, and selection.  Not for drawing.
   (%min-column-number :initarg :min-column-number :reader min-column-number)
   ;; This slot contains the column number of the leftmost known
   ;; non-whitespace character of the parse result.  It may not be
   ;; entirely correct for the same reason as the preceding slot.
   (%max-column-number :initarg :max-column-number :reader max-column-number)
   ;; This slot contains the maximum line width of any line that is
   ;; part of the parse result.
   (%max-line-width :initarg :max-line-width :reader max-line-width)
   ;; This slot contains TRUE if and only if the START-LINE slot is
   ;; relative to some other line.
   (%relative-p :initarg :relative-p :accessor relative-p)
   (%children :initarg :children :accessor children)))

(defmethod initialize-instance :after ((object parse-result) &key)
  (let ((min-column-number (min (start-column object)
                                (end-column object)
                                (reduce #'min (children object)
                                        :initial-value 0
                                        :key #'min-column-number)))
        (max-column-number (max (start-column object)
                                (end-column object)
                                (reduce #'max (children object)
                                        :initial-value 0
                                        :key #'max-column-number))))
    (reinitialize-instance object
                           :min-column-number min-column-number
                           :max-column-number max-column-number)))

(defmethod print-object ((object parse-result) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~d,~d -> ~d,~d) rel: ~s"
            (start-line object)
            (start-column object)
            (if (relative-p object)
                (height object)
                (end-line object))
            (end-column object)
            (relative-p object))))

;;; Define an indirection for MAKE-INSTANCE for creating parse
;;; results.  The main purpose is so that the creation of parse
;;; results can be traced.
(defun make-parse-result (class &rest initargs)
  (apply #'make-instance class initargs))

(defclass expression-parse-result (parse-result)
  ((%expression :initarg :expression :accessor expression)))

(defclass no-expression-parse-result (parse-result)
  ())

(defclass comment-parse-result (no-expression-parse-result)
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
