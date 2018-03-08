(cl:in-package #:climacs-syntax-common-lisp)

(defun indent-slots (slots client)
  ;; FIXME: indent slots
  (declare (ignore slots client))
  nil)

(defun indent-defclass (wad client)
  (let ((arguments (split-wads (rest (children wad)))))
    (unless (null arguments)
      (destructuring-bind (class-name . remaining) arguments
        (if (zerop (start-line (first class-name)))
            (loop with indentation = (start-column (first class-name))
                  for wad in (rest class-name)
                  unless (zerop (start-line wad))
                    do (setf (indentation wad) indentation))
            (loop with indentation = (+ (start-column wad) 4)
                  for wad in class-name
                  unless (zerop (start-line wad))
                    do (setf (indentation wad) indentation)))
        (unless (null remaining)
          (destructuring-bind (superclasses . remaining) remaining
            (if (zerop (start-line (first superclasses)))
                (loop with indentation = (start-column (first superclasses))
                      for wad in (rest superclasses)
                      unless (zerop (start-line wad))
                        do (setf (indentation wad) indentation))
                (loop with indentation = (+ (start-column wad) 4)
                      for wad in superclasses
                      unless (zerop (start-line wad))
                        do (setf (indentation wad) indentation)))
            (unless (null remaining)
              (destructuring-bind (slots . options) remaining
                ;; FIXME: indent options.
                (declare (ignore options))
                (indent-slots slots client)))))))))
              
(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:defclass))) client)
  (indent-defclass wad client))
