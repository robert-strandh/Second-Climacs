(cl:in-package #:climacs-syntax-common-lisp)

(defun compute-setf-and-setq-indentation (wad client)
  (let ((arguments (rest (children wad))))
    (unless (null arguments)
      (let* ((first-argument (first arguments))
             (first-argument-column (start-column first-argument)))
        (unless (zerop (start-line first-argument))
          (setf (indentation first-argument)
                (+ (start-column wad) 2)))
        (cond ((typep first-argument 'expression-wad)
               ;; Since the first argument is an EXPRESSION-WAD we
               ;; need to make sure its children are indented
               ;; correctly.
               (compute-child-indentations first-argument client)
               ;; Keep only arguments beyond the first EXPRESSION-WAD.
               (pop arguments))
              (t
               ;; Indent everything up to and including the first
               ;; EXPRESSION-WAD.
               (setf arguments
                     (indent-up-to-and-including-expression
                      first-argument-column (rest arguments) client))))
        ;; When we come here, all arguments up to the first
        ;; EXPRESSION-WAD have been indented.  If there are remaining
        ;; arguments, then they start with the first value form.
        (loop with additional-indentation = 2
              until (null arguments)
              do (setf arguments
                       (indent-up-to-and-including-expression
                        (+ first-argument-column additional-indentation)
                        arguments
                        client))
                 (setf additional-indentation
                       (- 2 additional-indentation)))))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:setf))) client)
  (compute-setf-and-setq-indentation wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:setq))) client)
  (compute-setf-and-setq-indentation wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:psetf))) client)
  (compute-setf-and-setq-indentation wad client))
