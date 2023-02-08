(cl:in-package #:second-climacs-syntax-common-lisp)

;;; As usual, we don't really compute the indentation of the
;;; expression itself, in this case the declaration specifier.
;;; Instead, we compute the indentation of the sub-expressions of that
;;; expression.
(defgeneric compute-declaration-specifier-indentation
    (wad pawn client))

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

(defmethod compute-declaration-specifier-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:declaration))) client)
  (let* ((indentation-units
           (compute-indentation-units (children wad)))
         (indentations
           (compute-declaration-indentations indentation-units client)))
    (assign-indentation-of-wads-in-units indentation-units indentations)))

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

(defmethod compute-declaration-specifier-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:dynamic-extent))) client)
  (let* ((indentation-units
           (compute-indentation-units (children wad)))
         (indentations
           (compute-dynamic-extent-indentations indentation-units client)))
    (assign-indentation-of-wads-in-units indentation-units indentations)))

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

(defmethod compute-declaration-specifier-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:ignore))) client)
  (let* ((indentation-units
           (compute-indentation-units (children wad)))
         (indentations
           (compute-ignore-indentations indentation-units client)))
    (assign-indentation-of-wads-in-units indentation-units indentations)))

(defmethod compute-declaration-specifier-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:ignorable))) client)
  (let* ((indentation-units
           (compute-indentation-units (children wad)))
         (indentations
           (compute-ignore-indentations indentation-units client)))
    (assign-indentation-of-wads-in-units indentation-units indentations)))
