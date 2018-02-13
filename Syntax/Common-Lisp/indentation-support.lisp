(cl:in-package #:climacs-syntax-common-lisp)

;;; This function is called when we have a simple form, and we have no
;;; particular information about the operator, i.e. either the
;;; operator is a LAMBDA expression, a symbol that is know to be the
;;; name of a function, but the lambda list of of that function is
;;; very simple, or a symbol that we have no information about.
(defun indent-default-function-call (wad)
  (let ((children (children wad)))
    (unless (null (rest children))
      (let* ((first-child (first children))
             (first-child-start-column (start-column first-child)))
        (unless (zerop first-child-start-column)
          ;; If the start column of the first child is 0, that means
          ;; that the first child is positioned on the same line as
          ;; the operator, so we do not modify its indentation.  If it
          ;; is different from 0, then we set the indentation to be 2
          ;; columns with respect to the start column of the wad.
          (setf (indentation first-child) (+ 2 (start-column wad))))
        (loop for child in (rest children)
              unless (zerop (start-column child))
                ;; If the start column of the child is 0, that means
                ;; that this child is positioned on the same line as
                ;; the previous child, so we do not modify its
                ;; indentation.  If it is different from 0, then we
                ;; set the indentation so that it aligns with the
                ;; start column of the first child of the wad.
                do (setf (indentation child) first-child-start-column))))))

;;; We define a form to be SIMPLE if and only if it is a proper list,
;;; and if the elements of the list correspond exactly to the
;;; expression parse results of the children of WAD in the same order.

(defun children-correspond-p (form wad-children)
  (if (null form)
      (or (null wad-children)
          (and (not (typep (car wad-children)
                           'expression-wad))
               (children-correspond-p form (cdr wad-children))))
      (and (not (null wad-children))
           (if (typep (car wad-children) 'expression-wad)
               (and (eq (car form)
                        (expression (car wad-children)))
                    (children-correspond-p (cdr form)
                                           (cdr wad-children)))
               (children-correspond-p form (cdr wad-children))))))

(defun proper-list-p (object)
  (not (null (ignore-errors (list-length object)))))

(defun simple-form-p (wad)
  (let ((expression (expression wad)))
    (and (consp expression)
         (proper-list-p expression)
         (children-correspond-p expression (children wad)))))

;;; To indent the sub-forms of a composite form, we can not use the
;;; symbol of the operator of that form, because that symbol may not
;;; yet exist, and in fact, it may have a package prefix that is not
;;; the name of any existing package yet either.
;;;
;;; But we would like to use a generic function to implement
;;; indentation, so that we can use an EQL specializer for the
;;; operator and a CLIENT instance for client-specific indentation
;;; styles.  And we can not use strings for EQL specializers.
;;;
;;; We solve this problem with an idea due to Jan Moringer, namely to
;;; "intern" a pair consisting of a package name and a symbol name
;;; (both string) in a hash table, thereby creating a TOKEN object
;;; that can then be used as an EQL specializer.
;;;
;;; Currently, we don't take care of garbage that may accumulate in
;;; the form of methods that can never be invoked, but we intend to
;;; address this problem later.

;;; The class of the tokens that will be used as EQL specializers.  We
;;; include the name of the package and the name of the symbol for
;;; debugging purposes.
(defclass token ()
  ((%package-name :initarg :package-name :reader package-name)
   (%symbol-name :initarg :symbol-name :reader symbol-name)))

;;; A hash table mapping pairs to tokens.  Each pair is a CONS cell
;;; with the package name in the CAR slot and the symbol name in the
;;; CDR slot.
(defparameter *tokens* (make-hash-table :test #'equal))

;;; The generic function called in order to compute the relative
;;; indentation of the children of WAD.  Each method defines an EQL
;;; specializer on the TOKEN parameter.  Client code should specialize
;;; on its own CLIENT class.
(defgeneric compute-sub-form-indentations (wad token client))

(defun intern-token (package-name-designator symbol-name-designator)
  (let* ((package-name (string package-name-designator))
         (symbol-name (string symbol-name-designator))
         (key (cons package-name symbol-name)))
    (multiple-value-bind (existing-token present-p) (gethash key *tokens*)
      (if (null present-p)
          (setf (gethash key *tokens*)
                (make-instance 'token
                  :package-name package-name
                  :symbol-name symbol-name))
          existing-token))))

(defun find-token (package-name-designator symbol-name-designator)
  (let* ((package-name (string package-name-designator))
         (symbol-name (string symbol-name-designator))
         (key (cons package-name symbol-name)))
    (multiple-value-bind (existing-token present-p) (gethash key *tokens*)
      (if (null present-p)
          ;; FIXME: signal a specific condition.
          (error "Unknown token (~s .~s)" package-name symbol-name)
          existing-token))))

;;; This generic function is called on any wad.  It determines what
;;; kind of wad it is and either calls some specific indentation
;;; function, or some default one.
(defgeneric compute-child-indentations (wad client))
