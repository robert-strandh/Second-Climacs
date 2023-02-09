(cl:in-package #:second-climacs-syntax-common-lisp)

(defun indent-prog1-etc (wad client)
  (compute-indentation-single-distinguished
   wad
   (lambda (wad) (compute-child-indentations wad client))
   (lambda (indentation wads)
     (indent-body indentation wads client))))

(defun compute-indentation-after-last-sub-wad-prog1-etc (wad)
  (let ((arguments (split-wads (rest (children wad)))))
    (+ (start-column wad)
       (cond ((null arguments)
              ;; We indent the line as if it is going to contain the
              ;; distinguished argument.
              4)
             ((= (length arguments) 1)
              ;; We either have only non-expression wads in the first
              ;; argument, or the last one is an expression wad.
              (if (typep (first (last (first arguments))) 'expression-wad)
                  ;; Since we have an expression wad in the first
                  ;; argument, we indent the line as if the body of
                  ;; the form is going to to be supplied.
                  2
                  ;; We have only non-expression wads in the first
                  ;; argument.  We look for the last wad in the first
                  ;; argument that is by itself on a line.
                  (let ((last (find-if (lambda (wad)
                                         (zerop (start-line wad)))
                                       (first arguments)
                                       :from-end t)))
                    (if (null last)
                        ;; Then all the non-expression wads in the
                        ;; first argument are aligned after the
                        ;; operator.  We then indent according to the
                        ;; start column of the first one.
                        (start-column (first (first arguments)))
                        ;; Otherwise, we indent according to the start
                        ;; column of that last non-expression wad being
                        ;; first on its line.
                        (start-column last)))))
             (t
              ;; We have more than one argument.  We look for the last
              ;; wad in all the arguments except the last that is by
              ;; itself on a line.
              (let ((last (find-if (lambda (wad)
                                     (zerop (start-line wad)))
                                   (rest (children wad))
                                   :start (length (first arguments))
                                   :from-end t)))
                (if (null last)
                    2
                    (start-column last))))))))

(defmethod compute-form-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:prog1))) client)
  (indent-prog1-etc wad client))

(defmethod compute-form-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:multiple-value-prog1))) client)
  (indent-prog1-etc wad client))

(defmethod compute-form-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:multiple-value-call))) client)
  (indent-prog1-etc wad client))

(defmethod compute-form-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:catch))) client)
  (indent-prog1-etc wad client))

(defmethod compute-form-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:throw))) client)
  (indent-prog1-etc wad client))

(defmethod compute-form-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:unwind-protect))) client)
  (indent-prog1-etc wad client))

(defmethod compute-form-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:when))) client)
  (indent-prog1-etc wad client))

(defmethod compute-form-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:unless))) client)
  (indent-prog1-etc wad client))
