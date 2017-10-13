(cl:in-package #:climacs-syntax-common-lisp)

(defgeneric parse-result-to-cst (parse-result))

(defun children-match-sub-expressions-p (children sub-expressions)
  (and (= (length children) (length sub-expressions))
       (loop for child in children
             for sub-expression in sub-expressions
             always (eq sub-expression (expression child)))))

(defmethod parse-result-to-cst ((parse-result expression-parse-result))
  (let ((relevant-children (remove-if-not (lambda (x)
                                            (typep x 'expression-parse-result))
                                          (children parse-result)))
        (expression (expression parse-result)))
    (cond ((atom expression)
           (make-instance 'cst:atom-cst
             :raw expression
             :source (list (cons (start-line parse-result)
                                 (start-column parse-result))
                           (cons (height parse-result)
                                 (end-column parse-result)))))
          ((children-match-sub-expressions-p relevant-children expression)
           (loop with result = (cst:cst-from-expression nil)
                 for child in (reverse relevant-children)
                 do (setf result
                          (cst:cons (parse-result-to-cst child)
                                    result
                                    :source (list (cons (start-line child)
                                                        (start-column child))
                                                  (cons (height child)
                                                        (end-column child)))))
                          
                 finally (return result)))
          (t 
           (cst:cst-from-expression expression)))))
    
