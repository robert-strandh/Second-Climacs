(cl:in-package #:climacs-syntax-common-lisp)

(defgeneric indent-local-function-definition (wad client))

(defmethod indent-local-function-definition (wad client)
  (let ((children (children wad)))
    (unless (or (null children)
                (null (rest children)))
      (let (;; We start by computing the indentation for the the
            ;; lambda list.
            (body-wads (compute-distinguished-indentation
                        (second children) 4 #'indent-lambda-list)))
        (indent-body (+ (start-column wad) 2) body-wads client)))))

