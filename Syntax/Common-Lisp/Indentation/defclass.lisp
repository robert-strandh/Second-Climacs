(cl:in-package #:climacs-syntax-common-lisp)

(defun indent-slots (slots client)
  ;; FIXME: indent slots
  (declare (ignore slots client))
  nil)

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
                (indent-slots slots client)))))))))
              
(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:defclass))) client)
  (indent-defclass wad client))
