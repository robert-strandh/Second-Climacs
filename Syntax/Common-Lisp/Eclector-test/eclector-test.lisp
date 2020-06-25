(cl:in-package #:eclector-test)

(defclass client (eclector.parse-result:parse-result-client)
  ())

(defclass wad ()
  ((%source-position :initarg :source-position :reader source-position)))

(defclass expression-wad (wad)
  ((%expression :initarg :expression :reader expression)
   (%children :initarg :children :reader children)))

(defclass atomic-expression-wad (expression-wad)
  ((%expression :initarg :expression :reader expression))
  (:default-initargs :children '()))

(defclass compound-expression-wad (expression-wad)
  ())

(defclass unterminated-expression-wad

(defclass skipped-wad (wad)
  ())

(defclass comment-wad (skipped-wad)
  ())

(defclass line-comment-wad (comment-wad)
  ((%semicolon-count :initarg :semicolon-count :reader semicolon-count)))

(defclass block-comment-wad (comment-wad)
  ())

(defclass sharpsign-wad (skipped-wad)
  ((%expression :initarg :expression :reader expression)))

(defclass sharpsign-plus-wad (sharpsign-wad)
  ())

(defclass sharpsign-minus-wad (sharpsign-wad)
  ())

(defclass read-suppress-wad (skipped-wad)
  ())

(defclass reader-macro-wad (skipped-wad)
  ())

(defclass token ()
  ((%contents :initarg :contents :reader contents)))

(defclass symbol-token (token)
  ((%package-indicator :initarg :package-indicator :reader package-indicator)))

(defclass error-token (token)
  ())

(defmethod eclector.reader:interpret-token :around
    ((client client) input-stream token escape-ranges)
  (handler-case (call-next-method)
    (error () (make-instance 'error-token
                :contents token))))

(defmethod eclector.reader:interpret-symbol
    ((client client) input-stream package-indicator symbol-name internp)
  (if (or (eq package-indicator :keyword)
          (and (eq package-indicator :current)
               (eq *package* (find-package '#:keyword))))
      (intern symbol-name (find-package '#:keyword))
      (make-instance 'symbol-token
        :contents symbol-name
        :package-indicator package-indicator)))

(defmethod eclector.parse-result:make-expression-result
    ((client client) result children source)
  (make-instance 'expression-wad
    :expression result
    :children children
    :source-position source))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client client) stream reason source)
  (cond ((eq reason :block-comment)
         (make-instance 'block-comment-wad
           :source-position source))
        ((eq reason '*read-suppress*)
         (make-instance 'read-suppress-wad
           :source-position source))
        ((eq reason :reader-macro)
         (make-instance 'reader-macro-wad
           :source-position source))
        ((and (consp reason) (eq (car reason) :line-comment))
         (make-instance 'line-comment-wad
           :source-position source
           :semicolon-count (cdr reason)))
        ((and (consp reason) (eq (car reason) :sharpsign-plus))
         (make-instance 'sharpsign-plus-wad
           :source-position source
           :expression (cdr reason)))
        ((and (consp reason) (eq (car reason) :sharpsign-minus))
         (make-instance 'sharpsign-minus-wad
           :source-position source
           :expression (cdr reason)))
        (t
         (make-instance 'skipped-wad
           :source-position source))))

(defparameter *filename*
  "/home/strandh/Lisp/My-Projects/GIT-ified/Second-Climacs/Syntax/Common-Lisp/Eclector-test/input-test.lisp")

(defun eclector-test ()
  (let ((client (make-instance 'client)))
    (with-open-file (stream *filename* :direction :input)
      (eclector.parse-result:read client stream))))


