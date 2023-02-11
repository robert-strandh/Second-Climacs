(cl:in-package #:second-climacs-syntax-common-lisp)

(defclass client (eclector.parse-result:parse-result-client)
  ;; TODO it would be nicer not to store the stream in the client like
  ;; this, but the method on make-expression-result needs the stream
  ;; and cannot access it in other ways.
  ((stream* :initarg :stream*
            :reader  stream*)))

;;; Source positions

(defmethod eclector.base:source-position ((client client) (stream folio-stream))
  (cons (current-line-number stream) (current-item-number stream)))

;;; Feature expressions

(defmethod reader:check-feature-expression ((client client) (feature-expression t))
  t)

(defmethod reader:evaluate-feature-expression ((client client) (feature-expression t))
  nil)

;;; Token interpretation

(defmethod reader:interpret-token :around
    ((client client) input-stream token escape-ranges)
  (let ((result (call-next-method)))
    (typecase result
      (token  result)
      (number (make-instance 'numeric-token :characters token :value result))
      (t      (make-instance 'other-token :characters token)))))

(defmethod reader:interpret-symbol-token
    ((client client) input-stream token position-package-marker-1 position-package-marker-2)
  (multiple-value-bind (package-designator symbol-name)
      (cond ((null position-package-marker-1)
             (values *package* token))
            ((null position-package-marker-2)
             (values (if (= position-package-marker-1 0)
                         "KEYWORD"
                         (subseq token 0 position-package-marker-1))
                     (subseq token (1+ position-package-marker-1))))
            (t
             (values (subseq token 0 position-package-marker-1)
                     (subseq token (1+ position-package-marker-2)))))
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
                               :name (cl:symbol-name symbol))))))))

;;; Result construction

(defmethod eclector.base:source-position ((client client) (stream analyzer))
  (cons (current-line-number stream) (current-item-number stream)))

(defmethod eclector.base:make-source-range ((client client) (start t) (end t))
  (cons start end))

(defun make-result-wad (class stream source children
                        &rest extra-initargs &key &allow-other-keys)
  (destructuring-bind ((start-line . start-column) . (end-line . end-column))
      source
    (let* ((line-number    (current-line-number stream))
           (max-line-width (compute-max-line-width
                            stream start-line line-number '())))
      (apply #'make-wad class :start-line     start-line
                              :height         (- end-line start-line)
                              :start-column   start-column
                              :end-column     end-column
                              :relative-p     nil
                              :max-line-width max-line-width
                              :children       children
                              extra-initargs))))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client client) (stream t) (reason t) (source t))
  (flet ((make-it (class &rest extra-initargs)
           (apply #'make-result-wad class stream source '() extra-initargs)))
    (etypecase reason
      ((cons (eql :line-comment))
       (let ((result (make-it 'semicolon-comment-wad
                              :semicolon-count (cdr reason))))
         ;; Eclector returns the beginning of the following line as
         ;; the end of the comment.  But we want it to be the end of
         ;; the same line.  But I don't know how to do it correctly
         ;; (yet).
         result))
      ((eql :block-comment)          (make-it 'block-comment-wad))
      ((eql :reader-macro)           (make-it 'reader-macro-wad))
      ((eql *read-suppress*)         (make-it 'read-suppress-wad))
      ((cons (eql :sharpsign-plus))  (make-it 'sharpsign-plus-wad  :expression (cdr reason)))
      ((cons (eql :sharpsign-minus)) (make-it 'sharpsign-minus-wad :expression (cdr reason))))))

(defmethod eclector.parse-result:make-expression-result
    ((client client) (result t) (children t) (source t))
  (make-result-wad 'expression-wad (stream* client) source children
                   :expression result))
