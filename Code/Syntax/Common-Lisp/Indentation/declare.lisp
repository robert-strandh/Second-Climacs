(cl:in-package #:second-climacs-syntax-common-lisp)

;;; As usual, we don't really compute the indentation of the
;;; expression itself, in this case the declaration specifier.
;;; Instead, we compute the indentation of the sub-expressions of that
;;; expression.
(defgeneric compute-declaration-specifier-indentation
    (wad pawn client))

;;; This method is applicable when the caller specifies NIL for the
;;; pawn, meaning that we do not know the nature of the wad att all,
;;; only that it ought to be a declaration specifier.  It could be an
;;; atomic wad, in which case it should not have its indentation
;;; computed at all.  Or it could be a compound wad, but with an
;;; unknown declaration identifier, in which case we also do not
;;; compute its indentation.
(defmethod compute-declaration-specifier-indentation (wad (pawn null) client)
  (when (typep wad 'ip:cst-wad)
    (let ((expression (cst:raw wad)))
      (when (consp expression)
        (compute-declaration-specifier-indentation
         wad (first expression) client)))))

;;; This method is applicable when we are given a pawn, but there is
;;; no more specific method applicable, meaning we have not defined a
;;; method for this particular pawn.  So we do nothing.
(defmethod compute-declaration-specifier-indentation (wad (pawn pawn) client)
  nil)

;;; This macro is used to define a typical indentation method that
;;; computes indentation units and calls an automaton function.
(defmacro define-declaration-specifier-indentation-method (pawn automaton)
  `(defmethod compute-declaration-specifier-indentation
       (wad (pawn (eql (intern-pawn ,@pawn))) client)
     (compute-and-assign-indentations client wad ,automaton)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DECLARATION declaration specifier.

(define-indentation-automaton compute-declaration-indentations
  (tagbody
     (next)
     ;; The current wad must be the symbol DECLARATION, or else we
     ;; would not be here.
     (maybe-assign-indentation 1 3)
     (next)
     ;; The current wad represents a name.
   name
     (maybe-assign-indentation 3 3)
     (next)
     (go name)))

(define-declaration-specifier-indentation-method
    ('#:common-lisp '#:declaration) compute-declaration-indentations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DYNAMIC-EXTENT declaration specifier.

(define-indentation-automaton compute-dynamic-extent-indentations
  (tagbody
     (next)
     ;; The current wad must be the symbol DYNAMIC-EXTENT, or else we
     ;; would not be here.
     (maybe-assign-indentation 1 3)
     (next)
     ;; The current wad represents a name.
   name-or-function-name
     ;; We don't bother to recursively compute the indentation of an
     ;; argument of the form (FUNCTION <fn>), because it would be
     ;; useful only when <fn> is on a different line from the one
     ;; FUNCTION is on, and we think that is unlikely.
     (maybe-assign-indentation 3 3)
     (next)
     (go name-or-function-name)))

(define-declaration-specifier-indentation-method
    ('#:common-lisp '#:dynamic-extent) compute-dynamic-extent-indentations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IGNORE and IGNORABLE declaration specifiers.

(define-indentation-automaton compute-ignore-indentations
  (tagbody
     (next)
     ;; The current wad must be the symbol IGNORE or the symbol
     ;; IGNORABLE, or else we would not be here.
     (maybe-assign-indentation 1 3)
     (next)
     ;; The current wad represents a name.
   name-or-function-name
     ;; We don't bother to recursively compute the indentation of an
     ;; argument of the form (FUNCTION <fn>), because it would be
     ;; useful only when <fn> is on a different line from the one
     ;; FUNCTION is on, and we think that is unlikely.
     (maybe-assign-indentation 3 3)
     (next)
     (go name-or-function-name)))

(define-declaration-specifier-indentation-method
    ('#:common-lisp '#:ignore) compute-ignore-indentations)

(define-declaration-specifier-indentation-method
    ('#:common-lisp '#:ignorable) compute-ignore-indentations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INLINE and NOTINLINE declaration specifiers.

(define-indentation-automaton compute-inline-indentations
  (tagbody
     (next)
     ;; The current wad must be the symbol INLINE or the symbol
     ;; NOTINLINE, or else we would not be here.
     (maybe-assign-indentation 1 3)
     (next)
     ;; The current wad represents a name.
   function-name
     ;; We don't bother to recursively compute the indentation of an
     ;; argument of the form (SETF <name>), because it would be useful
     ;; only when <name> is on a different line from the one SETF is
     ;; on, and we think that is unlikely.
     (maybe-assign-indentation 3 3)
     (next)
     (go function-name)))

(define-declaration-specifier-indentation-method
    ('#:common-lisp '#:inline) compute-inline-indentations)

(define-declaration-specifier-indentation-method
    ('#:common-lisp '#:notinline) compute-inline-indentations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OPTIMIZE declaration specifiers.

(define-indentation-automaton compute-optimize-indentations
  (tagbody
     (next)
     ;; The current wad must be the symbol OPTIMIZE, or else we would
     ;; not be here.
     (maybe-assign-indentation 1 3)
     (next)
     ;; The current wad represents an optimize quality.
   quality
     ;; We don't bother to recursively compute the indentation of an
     ;; argument of the form (<quality> <n>), because it would be
     ;; useful only when <n> is on a different line from the one
     ;; <quality> is on, and we think that is unlikely.
     (maybe-assign-indentation 3 3)
     (next)
     (go quality)))

(define-declaration-specifier-indentation-method
    ('#:common-lisp '#:optimize) compute-optimize-indentations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SPECIAL declaration specifiers.

(define-indentation-automaton compute-special-indentations
  (tagbody
     (next)
     ;; The current wad must be the symbol SPECIAL, or else we would
     ;; not be here.
     (maybe-assign-indentation 1 3)
     (next)
     ;; The current wad represents a symbol naming a special variable.
   symbol
     (maybe-assign-indentation 3 3)
     (next)
     (go symbol)))

(define-declaration-specifier-indentation-method
    ('#:common-lisp '#:special) compute-special-indentations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPE and FTYPE declaration specifiers.

(define-indentation-automaton compute-type-indentations
  (tagbody
     (next)
     ;; The current wad must be the symbol TYPE or the symbol
     ;; FTYPE, or else we would not be here.
     (maybe-assign-indentation 1 3)
     (next)
     ;; The current wad represents a type specifier.
     (maybe-assign-indentation 4 2)
     (compute-type-specifier-indentation current-wad nil client)
     next
     ;; The current wad represents a name.
   name-or-function-name
     ;; We don't bother to recursively compute the indentation of an
     ;; argument of the form (SETF <name>), because it would be useful
     ;; only when <name> is on a different line from the one SETF is
     ;; on, and we think that is unlikely.
     (maybe-assign-indentation 2 2)
     (next)
     (go name-or-function-name)))

(define-declaration-specifier-indentation-method
    ('#:common-lisp '#:type) compute-type-indentations)

(define-declaration-specifier-indentation-method
    ('#:common-lisp '#:ftype) compute-type-indentations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shorthand type declarations.

(define-indentation-automaton compute-shorthand-indentations
  (tagbody
     (next)
     ;; The current wad is assumed to be a type specifier.
     (maybe-assign-indentation 1 3)
     (compute-type-specifier-indentation current-wad nil client)
     (next)
     ;; The current wad should represent a name.
   name
     (maybe-assign-indentation 3 3)
     (next)
     (go name)))

;;; This method is applicable when we are given something other than a
;;; pawn.  We then assume that we are dealing with the shorthand type
;;; declaration.

(defmethod compute-declaration-specifier-indentation (wad pawn client)
  (compute-and-assign-indentations client wad compute-shorthand-indentations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Indentation for the DECLARE expression.

(define-indentation-automaton compute-declare-indentations
  (tagbody
     (next)
     ;; The current wad represents the symbol DECLARE or the symbol
     ;; DECLAIM.
     (maybe-assign-indentation 1 3)
     (next)
   declaration-specifier
     (maybe-assign-indentation 3 3)
     (compute-declaration-specifier-indentation current-wad nil client)
     (next)
     (go declaration-specifier)))

(defun compute-declare-indentation (wad client)
  (compute-and-assign-indentations client wad compute-declare-indentations))

(define-form-indentation-method
    ('#:common-lisp '#:declaim) compute-declare-indentations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Indentation for the PROCLAIM expression.

(define-indentation-automaton compute-proclaim-indentations
  (tagbody
     (next)
     ;; The current wad represents the symbol PROCLAIM.
     (maybe-assign-indentation 1 3)
     (next)
   declaration-specifier
     (maybe-assign-indentation 3 3)
     (compute-declaration-specifier-indentation current-wad nil client)))

(define-form-indentation-method
    ('#:common-lisp '#:proclaim) compute-proclaim-indentations)
