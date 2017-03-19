(cl:in-package #:climacs-syntax-common-lisp)

(defclass token () ())

(defclass symbol-token (token)
  ((package-name
    :initform nil
    :initarg :package-name
    :reader package-name)
   (package-marker-count
    :initform 0
    :initarg :package-marker-count
    :reader package-marker-count)
   (name :initarg :name :reader name)))

(defclass other-token (token)
  ((%characters :initarg :characters :reader characters)))

(defclass numeric-token (other-token)
  ((%value :initarg :value :reader value)))

(defmethod print-object ((object token) stream)
  (print-unreadable-object (object stream)
    (format stream "~a" (characters object))))

(defmethod sicl-reader:interpret-symbol
    (token position-package-marker-1 position-package-marker-2 input-stream)
  (let ((package-designator nil)
        (symbol-name nil)
        (package-marker-count (cond ((null position-package-marker-1) 0)
                                    ((null position-package-marker-2) 1)
                                    (t 2))))
    (ecase package-marker-count
      (0
       (setf package-designator *package*)
       (setf symbol-name token))
      (1
       (setf package-designator
             (if (= position-package-marker-1 0)
                 "KEYWORD"
                 (subseq token 0 position-package-marker-1)))
       (setf symbol-name (subseq token (1+ position-package-marker-1))))
      (2
       (when (or (/= position-package-marker-2 (1+ position-package-marker-1))
                 (= position-package-marker-2 (1- (length token))))
         (return-from sicl-reader:interpret-symbol
           (make-instance 'other-token :characters token)))
       (setf package-designator (subseq token 0 position-package-marker-1))
       (setf symbol-name (subseq token (1+ position-package-marker-2)))))
    (let ((package (find-package package-designator)))
      (if (null package)
          (make-instance 'symbol-token
            :package-name package-designator
            :package-marker-count package-marker-count
            :name symbol-name)
          (multiple-value-bind (symbol status)
              (find-symbol symbol-name package)
            (if (null status)
                (make-instance 'symbol-token
                  :package-name (cl:package-name package)
                  :package-marker-count package-marker-count
                  :name symbol-name)
                symbol))))))
