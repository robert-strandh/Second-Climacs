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

(defun make-word-wads (stream source column-offset)
  (destructuring-bind ((start-line . start-column) . (end-line . end-column))
      source
    (declare (ignore end-line end-column))
    (let* ((cache             (folio stream))
           (contents          (line-contents cache start-line))
           (word              (make-array 0 :element-type 'character
                                            :adjustable   t
                                            :fill-pointer 0))
           (word-start-colunn (+ start-column column-offset))
           (words             '()))
      (flet ((terminatingp (character)
               (member character '(#\Space #\Newline
                                   #\. #\? #\! #\: #\, #\; #\( #\))))
             (commit (column)
               (when (plusp (length word))
                 (let ((source (cons (cons start-line word-start-colunn)
                                     (cons start-line column)))
                       (foundp (spell:english-lookup word)))
                   (push (make-result-wad 'word-wad stream source '()
                                          :misspelled (not foundp))
                         words)))
               (setf (fill-pointer word) 0
                     word-start-colunn   (1+ column))))
        (loop for column from start-column below (length contents)
              for character = (aref contents column)
              if (not (terminatingp character))
                do (vector-push-extend character word)
              else
                do (commit column)
              finally (commit column))
        (nreverse words)))))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client client) (stream t) (reason t) (source t))
  (flet ((make-it (class &rest extra-initargs)
           (apply #'make-result-wad class stream source '() extra-initargs)))
    (etypecase reason
      ((cons (eql :line-comment))
       ;; Eclector returns the beginning of the following line as the
       ;; end of the comment.  But we want it to be the end of the
       ;; same line.  But I don't know how to do it correctly (yet).
       (let* ((semicolon-count (cdr reason))
              (words           (make-word-wads stream source semicolon-count)))
         (make-result-wad 'semicolon-comment-wad
                          stream source words
                          :semicolon-count semicolon-count)))
      ((eql :block-comment)          (make-it 'block-comment-wad))
      ((eql :reader-macro)           (make-it 'reader-macro-wad))
      ((eql *read-suppress*)         (make-it 'read-suppress-wad))
      ((cons (eql :sharpsign-plus))  (make-it 'sharpsign-plus-wad  :expression (cdr reason)))
      ((cons (eql :sharpsign-minus)) (make-it 'sharpsign-minus-wad :expression (cdr reason))))))

(defmethod eclector.parse-result:make-expression-result
    ((client client) (result t) (children t) (source t))
  (make-result-wad 'expression-wad (stream* client) source children
                   :expression result))

(defmethod eclector.parse-result:make-expression-result
    ((client   client)
     (result   (eql eclector.parse-result:**definition**))
     (children t)
     (source   t))
  (let ((object (nth-value 1 (eclector.reader:labeled-object-state
                              client children))))
    (make-result-wad 'labeled-object-definition-wad (stream* client) source '()
                     :expression object)))

(defmethod eclector.parse-result:make-expression-result
    ((client   client)
     (result   (eql eclector.parse-result:**reference**))
     (children t)
     (source   t))
  (let ((object (nth-value 1 (eclector.reader:labeled-object-state
                              client children))))
    (make-result-wad 'labeled-object-reference-wad (stream* client) source '()
                     :expression object)))
