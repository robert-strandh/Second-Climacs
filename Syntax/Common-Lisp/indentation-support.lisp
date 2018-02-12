(cl:in-package #:climacs-syntax-common-lisp)

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
(defgeneric compute-child-indentations (wad token client))
