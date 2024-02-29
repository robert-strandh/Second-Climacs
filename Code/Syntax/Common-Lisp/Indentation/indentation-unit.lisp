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
  (if (typep wad 'ip:semicolon-comment-wad)
      0
      (ip:height wad)))

(defun wads-are-on-different-lines-p (wad1 wad2)
  (/= (+ (ip:absolute-start-line wad1) (effective-height wad1))
      (ip:absolute-start-line wad2)))

(defun compute-indentation-units (wads)
  (if (null wads)
      '()
      (let* ((result '())
             (first (first wads))
             (current-indentation-unit (list first))
             (seen-expression-wad-p (typep first 'ip:expression-wad)))
          (loop for wad in (rest wads)
                do (if (or (not (wads-are-on-different-lines-p
                                 wad (first current-indentation-unit)))
                           (not seen-expression-wad-p))
                       (push wad current-indentation-unit)
                       ;; Else we add the current indentation unit to
                       ;; the result, and start a new indentation
                       ;; unit.
                       (progn
                         (push (reverse current-indentation-unit) result)
                         (setf seen-expression-wad-p nil)
                         (setf current-indentation-unit (list wad))))
                   (when (typep wad 'ip:expression-wad)
                     (setf seen-expression-wad-p t)))
        (push (reverse current-indentation-unit) result)
        (reverse result))))

(defun wad-is-first-on-start-line-p (wad)
  (if (null (ip:left-sibling wad))
      (or (null (ip:parent wad))
          (/= (ip:absolute-start-line (ip:parent wad))
              (ip:absolute-start-line wad)))
      (/= (+ (ip:absolute-start-line (ip:left-sibling wad))
             (effective-height (ip:left-sibling wad)))
          (ip:absolute-start-line wad))))

(defun assign-indentation-of-wads-in-unit (indentation-unit indentation)
  (when (wad-is-first-on-start-line-p (first indentation-unit))
    (setf (ip:indentation (first indentation-unit)) indentation))
  (loop for (wad1 wad2) on indentation-unit
        until (null wad2)
        when (wads-are-on-different-lines-p wad1 wad2)
          do (setf (ip:indentation wad2) indentation)))

(defun assign-indentation-of-wads-in-units
    (indentation-units relative-indentations wad-column-number)
  (loop for indentation-unit in indentation-units
        for relative-indentation in relative-indentations
        for absolute-indentation = (+ relative-indentation wad-column-number)
        do (assign-indentation-of-wads-in-unit
            indentation-unit absolute-indentation)))

(defmacro with-indentation-automaton
    ((name indentation-units-variable) &body body)
  (let ((current-unit-variable (gensym))
        (indentations-variable (gensym))
        (remaining-units-variable (gensym))
        (seen-expression-wad-p-variable (gensym)))
    `(let ((,indentations-variable (list 1))
           (,current-unit-variable '())
           (,remaining-units-variable ,indentation-units-variable)
           (current-wad nil)
           (,seen-expression-wad-p-variable nil))
       (flet ((next ()
                (setf current-wad nil)
                (loop until (typep current-wad 'ip:expression-wad)
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
         (progn ,@body)
         ;; If we come here, it's that we didn't process all wads.
         (return-from ,name
           (reverse ,indentations-variable))))))

(defmacro define-indentation-automaton (name &body body)
  (let ((indentation-units-variable (gensym)))
    `(defun ,name (,indentation-units-variable client)
       (declare (ignorable client))
       (with-indentation-automaton (,name ,indentation-units-variable)
         ,@body))))

(defmacro compute-and-assign-indentations (client wad automaton)
  `(let* ((indentation-units (compute-indentation-units (ip:children ,wad)))
          (indentations (,automaton indentation-units ,client))
          (wad-column-number (ip:start-column ,wad)))
     (assign-indentation-of-wads-in-units
      indentation-units indentations wad-column-number)))

;;; This function computes the indentation of a uniform list of items,
;;; where each item should be indented according to
;;; COMPUTE-ELEMENT-INDENTATION.
(defun compute-list-indentation (wad client compute-element-indentation)
  (flet ((automaton (indentation-units client)
           (with-indentation-automaton (automaton indentation-units)
             (tagbody
                (next)
              element
                (maybe-assign-indentation 1 1)
                (funcall compute-element-indentation current-wad client)
                (next)
                (go element)))))
    (compute-and-assign-indentations client wad automaton)))
