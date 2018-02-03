(cl:in-package #:climacs-syntax-common-lisp)

;;; We define a form to be SIMPLE if and only if it is a proper list,
;;; and if the elements of the list correspond exactly to the
;;; expression parse results of the children of PARSE-RESULT in the
;;; same order.

(defun children-correspond-p (form-children parse-result-children)
  (if (null form-children)
      (or (null parse-result-children)
          (and (not (typep (car parse-result-children)
                           'expression-parse-result))
               (children-correspond-p form-children
                                      (cdr parse-result-children))))
      (and (not (null parse-result-children))
           (if (typep (car parse-result-children)
                      'expression-parse-result)
               (and (eq (car form-children)
                        (expression (car parse-result-children)))
                    (children-correspond-p (cdr form-children)
                                           (cdr parse-result-children)))
               (children-correspond-p form-children
                                      (cdr parse-result-children))))))
