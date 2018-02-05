(cl:in-package #:climacs-syntax-common-lisp)

;;; We define a form to be SIMPLE if and only if it is a proper list,
;;; and if the elements of the list correspond exactly to the
;;; expression parse results of the children of WAD in the same order.

(defun children-correspond-p (form wad-children)
  (if (null form)
      (or (null wad-children)
          (and (not (typep (car wad-children)
                           'expression-wad))
               (children-correspond-p form (cdr wad-children))))
      (and (not (null wad-children))
           (if (typep (car wad-children) 'expression-wad)
               (and (eq (car form)
                        (expression (car wad-children)))
                    (children-correspond-p (cdr form)
                                           (cdr wad-children)))
               (children-correspond-p form (cdr wad-children))))))

(defun proper-list-p (object)
  (not (null (ignore-errors (list-length object)))))

(defun simple-form-p (wad)
  (let ((expression (expression wad)))
    (and (consp expression)
         (proper-list-p expression)
         (children-correspond-p expression (children wad)))))

(defun indent-default-function-call (wad)
  (let ((children (children wad)))
    (unless (null (rest children))
      (let* ((first-child (first children))
             (first-child-start-column (start-column first-child)))
        (unless (zerop first-child-start-column)
          (setf (indentation first-child) (+ 2 (start-column wad))))
        (loop for child in (rest children)
              unless (zerop (start-column child))
                do (setf (indentation child) first-child-start-column))))))
