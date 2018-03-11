(cl:in-package #:climacs-syntax-common-lisp)

;;; SLOTS is a list of wads, some of which may be expression wads and
;;; some of which may be wads of some other type.  An expression wad
;;; in the list then represents a single slot specifier.
(defun indent-slots (wads client)
  ;; Start by aligning every wad in the list that starts on its own
  ;; line with the first one.
  (align-with-first wads)
  ;; Then, for each expression wad in the list, compute the
  ;; indentation according to the rules of a single slot specifier.
  (loop for wad in wads
        when (typep wad 'expression-wad)
          do (indent-slot wad client)))

(defun indent-defclass (wad client)
  (let ((arguments (split-wads (rest (children wad)))))
    (unless (null arguments)
      (destructuring-bind (class-name . remaining) arguments
        (align-or-indent class-name  (+ (start-column wad) 4))
        (unless (null remaining)
          (destructuring-bind (superclasses . remaining) remaining
            (align-or-indent superclasses (+ (start-column wad) 4))
            (unless (null remaining)
              (destructuring-bind (slots . options) remaining
                ;; FIXME: indent options.
                (declare (ignore options))
                (align-or-indent slots (+ (start-column wad) 2))
                (let ((last (first (last slots))))
                  (when (typep last 'expression-wad)
                    (indent-slots (children last) client)))))))))))
              
(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:defclass))) client)
  (indent-defclass wad client))
