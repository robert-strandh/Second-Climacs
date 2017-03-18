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
  (cond ((null position-package-marker-1)
         (multiple-value-bind (symbol status)
             (find-symbol token *package*)
           (make-instance 'symbol-token
             :package-name
             (if (null status)
                 (cl:package-name *package*)
                 (cl:package-name (symbol-package symbol)))
             :package-marker-count 0
             :name token)))
        ((null position-package-marker-2)
         (cond ((= position-package-marker-1 (1- (length token)))
                (make-instance 'symbol-token
                  :package-name (subseq token 0 (1- (length token)))
                  :package-marker-count 1
                  :name ""))
               ((= position-package-marker-1 0)
                (make-instance 'symbol-token
                  :package-name "KEYWORD"
                  :package-marker-count 1
                  :name (subseq token 1)))
               (t
                (multiple-value-bind (symbol status)
                    (find-symbol
                     (subseq token (1+ position-package-marker-1))
                     (subseq token 0 position-package-marker-1))
                  (cond ((null symbol)
                         (make-instance 'symbol-token
                           :package-name
                           (subseq token 0 position-package-marker-1)
                           :package-marker-count 2
                           :name
                           (subseq token (1+ position-package-marker-1))))
                        ((eq status :internal)
                         (make-instance 'symbol-token
                           :package-name
                           (subseq token 0 position-package-marker-1)
                           :package-marker-count 2
                           :name
                           (subseq token (1+ position-package-marker-1))))
                        (t
                         (make-instance 'symbol-token
                           :package-name
                           (subseq token 0 position-package-marker-1)
                           :package-marker-count 2
                           :name
                           (subseq token (1+ position-package-marker-1)))))))))
        (t
         (if (= position-package-marker-1 (1- (length token)))
             (make-instance 'symbol-token
               :package-name
               (subseq token 0 position-package-marker-1)
               :package-marker-count 2
               :name
               (subseq token (1+ position-package-marker-2)))
             (make-instance 'symbol-token
               :package-name
               (subseq token 0 position-package-marker-1)
               :package-marker-count 2
               :name
               (subseq token (1+ position-package-marker-2)))))))
