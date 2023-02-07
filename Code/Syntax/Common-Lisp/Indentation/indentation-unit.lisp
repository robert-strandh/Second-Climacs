(cl:in-package #:second-climacs-syntax-common-lisp)

;;; Indentation is computed only for wads that start a line.

;;; An indentation unit is a list of some sub-sequence of the child
;;; wads of some expression wad.  The children of an expression wad
;;; are divided into non-overlapping indentation units that together
;;; contain every such child.
;;;
;;; If some indentation unit U contains an EXPRESSION-WAD, then let W
;;; be the first such wad in U.  Then there is no wad following W in U
;;; that starts a line.  For an expression wad V following W in U, its
;;; children have their indentations computed only relative to the
;;; start column of V.  Wads preceding W in U may either start a line,
;;; or not.  Those that start a line have their indentation computed
;;; according to the rule for W, and the rule for W is determined by:
;;;
;;;   * the operator of the parent wad of W,
;;;   * the number and nature of the expression wads preceding it in
;;;     the list of siblings, and
;;;   * sometimes by the nature of W itself.
;;;
;;; An example of the last possibility is that the indentation of a
;;; declaration or a documentation string in the body of a function
;;; may be computed differently from a form in that body.
;;;
;;; If some indentation unit U does not contain an EXPRESSION-WAD,
;;; then it is the last indentation unit in the list of indentation
;;; units computed for the children of some expression wad.  The
;;; computation of the indentation for the wads of U that start a line
;;; is determined by:
;;;
;;;   * the operator of the parent wad of W, and
;;;   * the number and nature of the expression wads preceding it in
;;;     the list of siblings.
;;;   
;;; Here, since no expression wad exists, some default rule must be
;;; applied.  For example, in the body of a function, if no
;;; declaration and no documentation string precedes U, then the
;;; default rule could be to compute the indentation as if a body form
;;; followed.

;;; This function is required because semicolon comment wads actually
;;; have a height of 1.
(defun effective-height (wad)
  (if (typep wad 'semicolon-comment-wad)
      0
      (height wad)))

(defun wads-are-on-different-lines-p (wad1 wad2)
  (/= (effective-height wad1) (start-line wad2)))

(defun compute-indentation-units (wads)
  (if (null wads)
      '()
      (let* ((result '())
             (first (first wads))
             (current-indentation-unit (list first))
             (seen-expression-wad-p (typep first 'expression-wad)))
          (loop for wad in (rest wads)
                do (if (or (= (start-line wad)
                              (effective-height (first current-indentation-unit)))
                           (not seen-expression-wad-p))
                       (push wad current-indentation-unit)
                       ;; Else we add the current indentation unit to
                       ;; the result, and start a new indentation
                       ;; unit.
                       (progn
                         (push (reverse current-indentation-unit) result)
                         (setf seen-expression-wad-p nil)
                         (setf current-indentation-unit (list wad))))
                   (when (typep wad 'expression-wad)
                     (setf seen-expression-wad-p t)))
        (push (reverse current-indentation-unit) result)
        (reverse result))))

(defun assign-indentation-of-wads-in-unit
    (indentation-unit indentation)
  ;; Always assign indentation of the first wad in the unit.
  (setf (indentation (first indentation-unit)) indentation)
  (loop for (wad1 wad2) on indentation-unit
        until (null wad2)
        when (wads-are-on-different-lines-p wad1 wad2)
          do (setf (indentation wad2) indentation)))

(defun assign-indentation-of-wads-in-units (indentation-units indentations)
  (loop for indentation-unit in indentation-units
        for indentation in indentations
        do (assign-indentation-of-wads-in-unit indentation-unit indentation)))

(defmacro define-indentation-automaton (name &body body)
  (let ((indentation-units-variable (gensym))
        (current-unit-variable (gensym))
        (indentations-variable (gensym))
        (remaining-units-variable (gensym))
        (seen-expression-wad-p-variable (gensym)))
    `(defun ,name (,indentation-units-variable client)
       (let ((,indentations-variable (list 1))
             (,current-unit-variable '())
             (,remaining-units-variable ,indentation-units-variable)
             (current-wad nil)
             (,seen-expression-wad-p-variable nil))
         (flet ((next ()
                  (setf current-wad nil)
                  (loop until (typep current-wad 'expression-wad)
                        do (when (null ,current-unit-variable)
                        (setf ,seen-expression-wad-p-variable nil)
                        (if (null ,remaining-units-variable)
                            (return-from ,name
                              (reverse ,indentations-variable))
                            (setf ,current-unit-variable
                                  (pop ,remaining-units-variable))))
                      (setf current-wad (pop ,current-unit-variable))))
                (maybe-assign-indentation (indentation next-default)
                  (unless ,seen-expression-wad-p-variable
                    (setf ,seen-expression-wad-p-variable t)
                    (setf (first ,indentations-variable) indentation)
                    (push next-default ,indentations-variable))))
           ,@body)))))
