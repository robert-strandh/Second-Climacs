(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-slot-specifier-indentations
  (tagbody
     (next)
     ;; The current wad ought to be the slot name.
     (maybe-assign-indentation 1 3)
     (next)
   slot-option
     ;; The current wad ought to be a slot-option name.  We indent the
     ;; slot-option name by 2 positions compared to the slot name.
     (maybe-assign-indentation 3 5)
     ;; Most slot options have a name or a form as its value, and it
     ;; won't hurt to compute the indentation of a name as if it were
     ;; a form.  One option, though, is different, namely :TYPE, which
     ;; takes a type specifier as a value, and we would like to indent
     ;; that type specifier as such.
     (let ((slot-option-name-wad current-wad))
       (next)
       (maybe-assign-indentation 5 3)
       (if (wad-represents-symbol-p slot-option-name-wad :type)
           (compute-type-specifier-indentation current-wad nil client)
           (compute-form-indentation current-wad nil client))
       (next)
       (go slot-option))))

;;; Compute the indentation of a single slot specifier.
(defun compute-slot-specifier-indentation (wad client)
  (compute-and-assign-indentations
   client wad compute-slot-specifier-indentations))

;;; Compute the indentation of a list of slot specifiers.
(defun compute-slot-specifiers-indentation (wad client)
  (compute-list-indentation
   wad client #'compute-slot-specifier-indentation))

(defgeneric compute-class-option-indentation (wad pawn client))

(defmethod compute-class-option-indentation (wad pawn client)
  nil)

;;; This method is applicable when the caller specifies NIL for the
;;; pawn, meaning that we do not know the nature of the wad att all,
;;; only that it ought to be a class option.  It could be an atomic
;;; wad, in which case it should not have its indentation computed at
;;; all.  Or it could be a compound wad, but with an unknown
;;; class-option name, in which case we also do not compute its
;;; indentation.
(defmethod compute-class-option-indentation
    ((wad ip:cst-wad) (pawn null) client)
  (when (simple-form-p wad)
    (let ((first-child (first (ip:children wad))))
      (when (and (typep first-child 'ip:cst-wad)
                 (typep (cst:raw first-child) 'ip:symbol-token))
        (let* ((token (cst:raw first-child))
               (pawn (find-pawn (ip:package-name token) (ip:name token))))
          (unless (null pawn)
            (compute-class-option-indentation wad pawn client)))))))

;;; This method is applicable when we are given either something that
;;; is not a pawn, or a pawn that has no method associated with it.
;;; So we do nothing.
(defmethod compute-class-option-indentation (wad pawn client)
  nil)

(define-indentation-automaton compute-defclass-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to be the class name.
     (maybe-assign-indentation 6 4)
     (next)
     ;; The current wad ought to be the list of superclasses.
     (maybe-assign-indentation 4 2)
     (compute-list-indentation current-wad client (constantly nil))
     (next)
     ;; The current wad ought to be the list of slot specifiers.
     (maybe-assign-indentation 2 2)
     (compute-slot-specifiers-indentation current-wad client)
     (next)
   class-option
     (maybe-assign-indentation 2 2)
     (compute-class-option-indentation current-wad nil client)
     (next)
     (go class-option)))

(define-form-indentation-method
    ('#:common-lisp '#:defclass)  compute-defclass-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:define-condition)  compute-defclass-indentations)

;;; This macro is used to define a typical indentation method that
;;; computes indentation units and calls an automaton function.
(defmacro define-class-option-indentation-method (pawn automaton)
  `(defmethod compute-class-option-indentation
       (wad (pawn (eql (intern-pawn ,@pawn))) client)
     (compute-and-assign-indentations client wad ,automaton)))

(define-indentation-automaton compute-default-initargs-indentations
  (tagbody
     (next)
     ;; The current wad is the symbol :DEFAULT-INITARGS.
     (maybe-assign-indentation 1 3)
     (next)
   initarg-name
     ;; The current wad ought to be the name of an initarg.
     (maybe-assign-indentation 3 5)
     (next)
     ;; The current wad ought to be the value form of an initarg.
     (maybe-assign-indentation 5 3)
     (compute-form-indentation current-wad nil client)
     (next)
     (go initarg-name)))

(define-class-option-indentation-method
    ('#:keyword '#:default-initargs) compute-default-initargs-indentations)

(define-indentation-automaton compute-report-indentations
  (tagbody
     (next)
     ;; The current wad is the symbol :REPORT.
     (maybe-assign-indentation 1 3)
     (next)
     ;; The current wad can be a string, a symbol, or a lambda
     ;; expression.  In either case, we can indent it as a form.
     (maybe-assign-indentation 3 3)
     (compute-form-indentation current-wad nil client)))

(define-class-option-indentation-method
    ('#:keyword '#:report) compute-report-indentations)
