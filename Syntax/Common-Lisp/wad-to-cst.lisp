(cl:in-package #:climacs-syntax-common-lisp)

(defgeneric wad-to-cst (wad))

(defun children-match-sub-expressions-p (children sub-expressions)
  (and (= (length children) (length sub-expressions))
       (loop for child in children
             for sub-expression in sub-expressions
             always (eq sub-expression (expression child)))))

(defmethod wad-to-cst ((wad expression-wad))
  (let ((relevant-children (remove-if-not (lambda (x)
                                            (typep x 'expression-wad))
                                          (children wad)))
        (expression (expression wad)))
    (cond ((atom expression)
           (make-instance 'cst:atom-cst
             :raw expression
             :source (list (cons (start-line wad)
                                 (start-column wad))
                           (cons (height wad)
                                 (end-column wad)))))
          ((children-match-sub-expressions-p relevant-children expression)
           (loop with result = (cst:cst-from-expression nil)
                 for child in (reverse relevant-children)
                 do (setf result
                          (cst:cons (wad-to-cst child)
                                    result
                                    :source (list (cons (start-line child)
                                                        (start-column child))
                                                  (cons (height child)
                                                        (end-column child)))))
                          
                 finally (return result)))
          (t 
           (cst:cst-from-expression expression)))))
    
