(cl:in-package #:second-climacs-syntax-common-lisp)

(defmethod base:buffer ((analyzer ip:analyzer))
  (ip:buffer analyzer))

(defclass view (base:view) ())

(defmethod initialize-instance :after ((instance view) &key buffer)
  (let* ((cache (make-instance 'ip:cache :cluffer-buffer buffer))
         (analyzer (make-instance 'ip:analyzer :lines (ip:lines cache)
                                               :cache cache
                                               :buffer buffer)))
    (reinitialize-instance instance :analyzer analyzer)))

(defmethod base:view-name ((view view))
  "Common Lisp")

(setf (base:view-class "lisp") 'view
      (base:view-class "asd")  'view)
