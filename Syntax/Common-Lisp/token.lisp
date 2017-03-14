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

(defmethod sicl-reader:interpret-token
    (token token-escapes (input-stream folio-stream))
  (sicl-reader::convert-according-to-readtable-case token token-escapes)
  (let ((length (length token))
	(sign 1)
	(decimal-mantissa 0)
	(mantissa/numerator 0)
	(denominator 0)
	(fraction-numerator 0)
	(fraction-denominator 1)
	(exponent-sign 1)
	(exponent 0)
	(exponent-marker nil)
	(position-package-marker-1 nil)
	(position-package-marker-2 nil)
	(index -1))
    (tagbody
     start
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (multiple-value-bind (symbol status)
                 (find-symbol token *package*)
               (make-instance 'symbol-token
                 :package-name
                 (if (null status)
                     (cl:package-name *package*)
                     (cl:package-name (symbol-package symbol)))
                 :package-marker-count 0
                 :name token)))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((eql char #\+)
		    (go sign))
		   ((eql char #\-)
		    (setf sign -1)
		    (go sign))
		   ((digit-char-p char)
		    (setf decimal-mantissa
			  (+ (* decimal-mantissa 10.)
			     (digit-char-p char)))
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go decimal-integer))
		   ((digit-char-p char *read-base*)
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go integer))
		   ((eql char #\.)
		    (go dot))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     sign
       ;; sign
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (multiple-value-bind (symbol status)
                 (find-symbol token *package*)
               (make-instance 'symbol-token
                 :package-name
                 (if (null status)
                     (cl:package-name *package*)
                     (cl:package-name (symbol-package symbol)))
                 :package-marker-count 0
                 :name token)))
           (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf decimal-mantissa
			  (+ (* decimal-mantissa 10.)
			     (digit-char-p char)))
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go decimal-integer))
		   ((digit-char-p char *read-base*)
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go integer))
		   ((eql char #\.)
		    (go sign-dot))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     dot
       (incf index)
       (if (= length index)
	   (if *consing-dot-allowed-p*
	       (return-from sicl-reader:interpret-token
		 *consing-dot*)
	       (error 'invalid-context-for-consing-dot
		      :stream input-stream))
	   (if (= length index)
	   (return-from sicl-reader:interpret-token
             (make-instance 'numeric-token
               :characters token
               :value (* sign mantissa/numerator)))
           (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf fraction-numerator
			  (+ (* fraction-numerator 10)
			     (digit-char-p char)))
		    (setf fraction-denominator
			  (* fraction-denominator 10))
		    (go float-no-exponent))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol))))))
     sign-dot
       ;; sign decimal-point
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (multiple-value-bind (symbol status)
                 (find-symbol token *package*)
               (make-instance 'symbol-token
                 :package-name
                 (if (null status)
                     (cl:package-name *package*)
                     (cl:package-name (symbol-package symbol)))
                 :package-marker-count 0
                 :name token)))
           (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf fraction-numerator
			  (+ (* fraction-numerator 10)
			     (digit-char-p char)))
		    (setf fraction-denominator
			  (* fraction-denominator 10))
		    (go float-no-exponent))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     decimal-integer
       ;; [sign] decimal-digit+
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (make-instance 'numeric-token
               :characters token
               :value (* sign mantissa/numerator)))
           (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((eql char #\.)
		    (go decimal-integer-final))
		   ((digit-char-p char)
		    (setf decimal-mantissa
			  (+ (* decimal-mantissa 10.)
			     (digit-char-p char)))
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go decimal-integer))
		   ((digit-char-p char *read-base*)
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go integer))
		   ((eql char #\/)
		    (go ratio-start))
		   ((member char '(#\e #\E #\f #\F #\s #\S #\d #\D #\l #\L))
		    (setf exponent-marker char)
		    (go float-exponent-start))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     decimal-integer-final
       ;; [sign] decimal-digit+ decimal-point
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (make-instance 'numeric-token
               :characters token
               :value (* sign decimal-mantissa)))
           (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf fraction-numerator
			  (+ (* fraction-numerator 10)
			     (digit-char-p char)))
		    (setf fraction-denominator
			  (* fraction-denominator 10))
		    (go float-no-exponent))
		   ((member char '(#\e #\E #\f #\F #\s #\S #\d #\D #\l #\L))
		    (setf exponent-marker char)
		    (go float-exponent-start))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     integer
       ;; [sign] digit+
       ;; At least one digit is not decimal.
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (make-instance 'numeric-token
               :characters token
               :value (* sign mantissa/numerator)))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char *read-base*)
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go integer))
		   ((eql char #\/)
		    (go ratio-start))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     ratio-start
       ;; [sign] digit+ /
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (multiple-value-bind (symbol status)
                 (find-symbol token *package*)
               (make-instance 'symbol-token
                 :package-name
                 (if (null status)
                     (cl:package-name *package*)
                     (cl:package-name (symbol-package symbol)))
                 :package-marker-count 0
                 :name token)))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char *read-base*)
		    (setf denominator
			  (+ (* denominator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go ratio))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     ratio
       ;; [sign] digit+ / digit+
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (make-instance 'numeric-token
               :characters token
               :value (/ mantissa/numerator denominator)))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char *read-base*)
		    (setf denominator
			  (+ (* denominator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go ratio))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     float-no-exponent
       ;; [sign] decimal-digit* decimal-point decimal-digit+
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (make-instance 'numeric-token
               :characters token
               :value (float (+ mantissa/numerator
                                (/ fraction-numerator fraction-denominator)))))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf fraction-numerator
			  (+ (* fraction-numerator 10)
			     (digit-char-p char)))
		    (setf fraction-denominator
			  (* fraction-denominator 10))
		    (go float-no-exponent))
		   ((member char '(#\e #\E #\f #\F #\s #\S #\d #\D #\l #\L))
		    (setf exponent-marker char)
		    (go float-exponent-start))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     float-exponent-start
       ;; [sign] decimal-digit+ exponent-marker
       ;; or
       ;; [sign] decimal-digit* decimal-point decimal-digit+ exponent-marker
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (multiple-value-bind (symbol status)
                 (find-symbol token *package*)
               (make-instance 'symbol-token
                 :package-name
                 (if (null status)
                     (cl:package-name *package*)
                     (cl:package-name (symbol-package symbol)))
                 :package-marker-count 0
                 :name token)))
           (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((eq char #\+)
		    (go float-exponent-sign))
		   ((eq char #\-)
		    (setf exponent-sign -1)
		    (go float-exponent-sign))
		   ((digit-char-p char)
		    (setf exponent
			  (+ (* exponent 10)
			     (digit-char-p char)))
		    (go float-exponent))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     float-exponent-sign
       ;; [sign] decimal-digit+ exponent-marker sign
       ;; or
       ;; [sign] decimal-digit* decimal-point decimal-digit+ exponent-marker sign
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (multiple-value-bind (symbol status)
                 (find-symbol token *package*)
               (make-instance 'symbol-token
                 :package-name
                 (if (null status)
                     (cl:package-name *package*)
                     (cl:package-name (symbol-package symbol)))
                 :package-marker-count 0
                 :name token)))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf exponent
			  (+ (* exponent 10)
			     (digit-char-p char)))
		    (go float-exponent))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     float-exponent
       ;; [sign] decimal-digit+ exponent-marker [sign] digit+
       ;; or
       ;; [sign] decimal-digit* decimal-point decimal-digit+
       ;; exponent-marker [sign] digit+
       (incf index)
       (if (= length index)
	   (return-from sicl-reader:interpret-token
             (make-instance 'numeric-token
               :characters token
               :value (coerce (* (+ mantissa/numerator
                                    (/ fraction-numerator
                                       fraction-denominator))
                                 (expt 10 (* exponent-sign exponent)))
                       (ecase exponent-marker
                         ((#\e #\E) 'float)
                         ((#\f #\F)'single-float)
                         ((#\s #\S) 'short-float)
                         ((#\d #\D) 'double-float)
                         ((#\l #\L) 'long-float)))))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf exponent
			  (+ (* exponent 10)
			     (digit-char-p char)))
		    (go float-exponent))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     symbol
       ;; a sequence of symbols denoting a valid symbol name, except
       ;; that the last character might be a package marker.
       (incf index)
       (if (= length index)
	   (cond ((null position-package-marker-1)
		  (return-from sicl-reader:interpret-token
                    (multiple-value-bind (symbol status)
                        (find-symbol token *package*)
                      (make-instance 'symbol-token
                        :package-name
                        (if (null status)
                            (cl:package-name *package*)
                            (cl:package-name (symbol-package symbol)))
                        :package-marker-count 0
                        :name token))))
		 ((null position-package-marker-2)
		  (cond ((= position-package-marker-1 (1- length))
			 (error 'symbol-name-must-not-end-with-package-marker
				:stream input-stream
				:desired-symbol token))
			((= position-package-marker-1 0)
			 (return-from sicl-reader:interpret-token
                           (make-instance 'symbol-token
                             :package-name "KEYWORD"
                             :package-marker-count 1
                             :name (subseq token 1))))
                        (t
			 (multiple-value-bind (symbol status)
			     (find-symbol
			      (subseq token (1+ position-package-marker-1))
			      (subseq token 0 position-package-marker-1))
			   (cond ((null symbol)
				  (error 'symbol-does-not-exist
					 :stream input-stream
					 :desired-symbol token))
				 ((eq status :internal)
				  (error 'symbol-is-not-external
					 :stream input-stream
					 :desired-symbol token))
				 (t
				  (return-from sicl-reader:interpret-token
                                    (make-instance 'symbol-token
                                      :package-name
                                      (if (null status)
                                          (subseq token 0 position-package-marker-1)
                                          (cl:package-name (symbol-package symbol)))
                                      :package-marker-count 1
                                      :name
                                      (subseq token (1+ position-package-marker-1))))))))))
		 (t
		  (if (= position-package-marker-1 (1- length))
		      (error 'symbol-name-must-not-end-with-package-marker
			     :stream input-stream
			     :desired-symbol token)
		      (return-from sicl-reader:interpret-token
                        (multiple-value-bind (symbol status)
                            (find-symbol (subseq token (1+ position-package-marker-2))
                                         (subseq token 0 position-package-marker-1))
                          (make-instance 'symbol-token
                            :package-name
                            (if (null status)
                                (subseq token 0 position-package-marker-1)
                                (cl:package-name (symbol-package symbol)))
                            :package-marker-count 2
                            :name
                            (subseq token (1+ position-package-marker-2))))))))
           (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((eq char #\:)
		    (cond ((null position-package-marker-1)
			   (setf position-package-marker-1 index))
			  ((null position-package-marker-2)
			   (cond ((/= position-package-marker-1 (1- index))
				  (error 'two-package-markers-must-be-adjacent
					 :stream input-stream
					 :desired-symbol token))
				 ((= position-package-marker-1 0)
				  (error 'two-package-markers-must-not-be-first
					 :stream input-stream
					 :desired-symbol token))
				 (t
				  (setf position-package-marker-2 index))))
			  (t
			   (error 'symbol-can-have-at-most-two-package-markers
				  :stream input-stream
				  :desired-symbol token)))
		    (go symbol))
		   (t
		    (go symbol))))))))
