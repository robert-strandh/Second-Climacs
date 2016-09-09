(cl:in-package #:climacs-syntax-common-lisp)

(defclass parse-result ()
  ((%start-line :initarg :start-line :accessor start-line)
   (%end-line :initarg :end-line :accessor end-line)
   (%start-column :initarg :start-column :accessor start-column)
   (%end-column :initarg :end-column :accessor end-column)
   (%children :initarg :children :accessor children)
   (%expression :initarg expression :accessor expression)))

(defclass analyzer ()
  ((%prefix :initform '() :accessor prefix)
   (%suffix :initform '() :accessor suffix)
   (%residue :initform '() :accessor residue)
   (%worklist :initform '() :accessor worklist)))

(defun suffix-to-prefix (analyzer)
  (with-accessors ((prefix prefix)
		   (suffix suffix))
      analyzer
    (assert (not (null suffix)))
    (let* ((first-cons-cell suffix)
	   (first-parse-result (first first-cons-cell)))
      (setf suffix (rest suffix))
      (unless (null suffix)
	;; Convert start-line of new first element of suffix
	;; to absolute line number by adding the absolute line
	;; number of the element we are removing.
	(incf (start-line (first suffix))
	      (start-line first-parse-result)))
      ;; Reuse the CONS cell rather than allocating a new one.
      (setf (rest first-cons-cell) prefix)
      (setf prefix first-cons-cell))))

(defun prefix-to-suffix (analyzer)
  (with-accessors ((prefix prefix)
		   (suffix suffix))
      analyzer
    (assert (not (null prefix)))
    (let* ((first-cons-cell prefix)
	   (first-parse-result (first first-cons-cell)))
      (setf prefix (rest prefix))
      (unless (null suffix)
	;; Convert start-line of current first element of suffix to
	;; relative line number by subtracting the absolute line
	;; number of the element we are adding.
	(decf (start-line (first suffix))
	      (start-line first-parse-result)))
      ;; Reuse the CONS cell rather than allocating a new one.
      (setf (rest first-cons-cell) suffix)
      (setf suffix first-cons-cell))))
