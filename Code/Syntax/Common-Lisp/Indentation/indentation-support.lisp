(cl:in-package #:second-climacs-syntax-common-lisp)

;;; As usual, we don't really compute the indentation of the
;;; expression itself, in this case the form.  Instead, we compute the
;;; indentation of the sub-expressions of that expression.
(defgeneric compute-form-indentation (wad pawn client))

;;; This function is called when we have a simple form, and we have no
;;; particular information about the operator, i.e. either the
;;; operator is a LAMBDA expression, a symbol that is know to be the
;;; name of a function, but the lambda list of of that function is
;;; very simple, or a symbol that we have no information about.
(defun indent-default-function-call (wad client)
  (let ((arguments (rest (ip:children wad))))
    (unless (null arguments)
      (let* ((first-child (first arguments))
             (first-child-start-column (ip:start-column first-child)))
        (unless (wads-are-on-different-lines-p wad first-child)
          ;; If the first child is positioned on the same line as the
          ;; operator, so we do not modify its indentation.  If the
          ;; two are different, then we set the indentation to be 2
          ;; columns with respect to the start column of the wad.
          (setf (ip:indentation first-child) (+ 2 (ip:start-column wad))))
        (loop for child in arguments
              for next in (rest arguments)
              unless (wads-are-on-different-lines-p child next)
                ;; If the two consecutive are positioned on the same
                ;; line, we do not modify the indentation.  of the
                ;; second one.  If they are on different lines, then
                ;; we set the indentation of teh second one so that it
                ;; aligns with the start column of the first child of
                ;; the wad.
                do (setf (ip:indentation child) first-child-start-column)
              do (compute-form-indentation child nil client))))))

;;; We define a form to be SIMPLE if and only if it is a proper list,
;;; and if the elements of the list correspond exactly to the
;;; expression wads of the children of WAD in the same order.

(defun children-correspond-p (form wad-children)
  (if (null form)
      (or (null wad-children)
          (and (not (typep (car wad-children)
                           'ip:expression-wad))
               (children-correspond-p form (cdr wad-children))))
      (and (not (null wad-children))
           (if (typep (car wad-children) 'ip:expression-wad)
               (and (eq (car form)
                        (ip:expression (car wad-children)))
                    (children-correspond-p (cdr form)
                                           (cdr wad-children)))
               (children-correspond-p form (cdr wad-children))))))

(defun proper-list-p (object)
  (not (null (ignore-errors (list-length object)))))

(defun simple-form-p (wad)
  (let ((expression (ip:expression wad)))
    (and (consp expression)
         (proper-list-p expression)
         (children-correspond-p expression (ip:children wad)))))

;;; To indent the sub-forms of a composite form, we can not use the
;;; symbol of the operator of that form, because that symbol may not
;;; yet exist, and in fact, it may have a package prefix that is not
;;; the name of any existing package yet either.
;;;
;;; But we would like to use a generic function to implement
;;; indentation, so that we can use an EQL specializer for the
;;; operator and a CLIENT instance for client-specific indentation
;;; styles.  And we can not use strings for EQL specializers.
;;;
;;; We solve this problem with an idea due to Jan Moringer, namely to
;;; "intern" a pair consisting of a package name and a symbol name
;;; (both string) in a hash table, thereby creating a PAWN object
;;; that can then be used as an EQL specializer.
;;;
;;; Currently, we don't take care of garbage that may accumulate in
;;; the form of methods that can never be invoked, but we intend to
;;; address this problem later.

;;; The class of the pawns that will be used as EQL specializers.  We
;;; include the name of the package and the name of the symbol for
;;; debugging purposes.
(defclass pawn ()
  ((%package-name :initarg :package-name :reader package-name)
   (%symbol-name :initarg :symbol-name :reader symbol-name)))

;;; A hash table mapping pairs to pawns.  Each pair is a CONS cell
;;; with the package name in the CAR slot and the symbol name in the
;;; CDR slot.
(defparameter *pawns* (make-hash-table :test #'equal))

;;; This generic function is called in order to determine the
;;; indentation of a line that is located after the last sub-wad in
;;; WAD.  Each method defines an EQL specializer on the PAWN
;;; parameter.  Client code should specialize on its own CLIENT class.
(defgeneric compute-indentation-after-last-sub-wad (wad pawn client))

(defun intern-pawn (package-name-designator symbol-name-designator)
  (let* ((package-name (string package-name-designator))
         (symbol-name (string symbol-name-designator))
         (key (cons package-name symbol-name)))
    (multiple-value-bind (existing-pawn present-p) (gethash key *pawns*)
      (if (null present-p)
          (setf (gethash key *pawns*)
                (make-instance 'pawn
                  :package-name package-name
                  :symbol-name symbol-name))
          existing-pawn))))

(defun find-pawn (package-name-designator symbol-name-designator)
  (let* ((package-name (string package-name-designator))
         (symbol-name (string symbol-name-designator))
         (key (cons package-name symbol-name)))
    (gethash key *pawns*)))

(defun wad-represents-symbol-p (wad symbol)
  (and (typep wad 'ip:expression-wad)
       (let ((expression (ip:expression wad)))
         (and (typep expression 'ip:symbol-token)
              (equal (ip:package-name expression)
                     (cl:package-name (symbol-package symbol)))
              (equal (ip:name expression)
                     (cl:symbol-name symbol))))))

(defun first-child-wad-represents-symbol-p (wad symbol)
  (and (typep wad 'ip:expression-wad)
       (let ((children (ip:children wad)))
         (and (consp children)
              (wad-represents-symbol-p (first children) symbol)))))

(defun indent-line (cache cursor)
  (let* ((line-number (cluffer:line-number cursor))
         (position (cluffer:cursor-position cursor))
         (wad (ip:find-wad-beginning-line cache line-number)))
    (unless (or (null wad) (null (ip:indentation wad)))
      (base:beginning-of-line cursor)
      (let ((start-column (ip:start-column wad))
            (indentation (ip:indentation wad)))
        (loop repeat (- start-column indentation)
              do (base:delete-item cursor))
        (loop repeat (- indentation start-column)
              do (base:insert-item cursor #\Space))
        (setf (cluffer:cursor-position cursor)
              (+ position (- indentation start-column)))))))

;;; Take a list of wads.  Use the column of the first one to align all
;;; the remaining ones that are on the beginning of a line.
(defun align-with-first (wads)
  (loop with indentation = (ip:start-column (first wads))
        for wad in wads
        for next in (rest wads)
        unless (wads-are-on-different-lines-p wad next)
          do (setf (ip:indentation wad) indentation)))

;;; Take a list of wads.  If the first wad is first on a line, then
;;; indent every wad according to INDENTATION.  If the first wad is
;;; NOT first on a line, then then indent the others like the first
;;; one.
(defun align-or-indent (wads reference-wad indentation)
  (if (wads-are-on-different-lines-p (first wads) reference-wad)
      (loop for wad in (cons reference-wad wads)
            for next in wads
            unless (wads-are-on-different-lines-p wad next)
              do (setf (ip:indentation next) indentation))
      (align-with-first wads)))
