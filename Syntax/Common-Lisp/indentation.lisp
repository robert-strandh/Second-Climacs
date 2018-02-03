(cl:in-package #:climacs-syntax-common-lisp)

;;; We define a form to be SIMPLE if and only if it is a proper list,
;;; and if the elements of the list correspond exactly to the
;;; expression parse results of the children of PARSE-RESULT in the
;;; same order.

(defun children-correspond-p (form parse-result-children)
  (if (null form)
      (or (null parse-result-children)
          (and (not (typep (car parse-result-children)
                           'expression-parse-result))
               (children-correspond-p form (cdr parse-result-children))))
      (and (not (null parse-result-children))
           (if (typep (car parse-result-children)
                      'expression-parse-result)
               (and (eq (car form)
                        (expression (car parse-result-children)))
                    (children-correspond-p (cdr form)
                                           (cdr parse-result-children)))
               (children-correspond-p form (cdr parse-result-children))))))

(defun proper-list-p (object)
  (not (null (ignore-errors (list-length object)))))

(defun simple-form-p (parse-result)
  (let ((expression (expression parse-result)))
    (and (consp expression)
         (proper-list-p expression)
         (children-correspond-p expression (children parse-result)))))
