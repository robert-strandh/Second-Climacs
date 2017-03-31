(cl:in-package #:climacs-syntax-common-lisp)

(defclass token () ())

(defclass symbol-token (token)
  ((%package-marker-1
    :initform nil
    :initarg :package-marker-1
    :reader package-marker-1)
   (%package-marker-2
    :initform nil
    :initarg :package-marker-2
    :reader package-marker-2)))

(defclass legal-symbol-token (symbol-token)
  ((%package-name
    :initform nil
    :initarg :package-name
    :reader package-name)
   (%name :initarg :name :reader name)))

(defclass illegal-symbol-token (symbol-token)
  ())

(defclass non-existing-symbol-token (legal-symbol-token)
  ())

(defclass non-existing-package-symbol-token (legal-symbol-token)
  ())

(defclass existing-symbol-token (legal-symbol-token)
  ())

(defclass other-token (token)
  ((%characters :initarg :characters :reader characters)))

(defclass numeric-token (other-token)
  ((%value :initarg :value :reader value)))

(defmethod print-object ((object other-token) stream)
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
           (make-instance 'illegal-symbol-token
             :package-marker-1 position-package-marker-1
             :package-marker-2 position-package-marker-2)))
       (setf package-designator (subseq token 0 position-package-marker-1))
       (setf symbol-name (subseq token (1+ position-package-marker-2)))))
    (let ((package (find-package package-designator)))
      (if (null package)
          (make-instance 'non-existing-package-symbol-token
            :package-name package-designator
            :package-marker-1 position-package-marker-1
            :package-marker-2 position-package-marker-2
            :name symbol-name)
          (multiple-value-bind (symbol status)
              (find-symbol symbol-name package)
            (if (null status)
                (make-instance 'non-existing-symbol-token
                  :package-name (cl:package-name package)
                  :package-marker-1 position-package-marker-1
                  :package-marker-2 position-package-marker-2
                  :name symbol-name)
                (make-instance 'existing-symbol-token
                  :package-name (cl:package-name (symbol-package symbol))
                  :package-marker-1 position-package-marker-1
                  :package-marker-2 position-package-marker-2
                  :name (symbol-name symbol))))))))
