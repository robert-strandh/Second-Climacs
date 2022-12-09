(cl:in-package #:climacs-syntax-common-lisp)

;;; This generic function is called on any wad.  It determines what
;;; kind of wad it is and either calls some specific indentation
;;; function, or some default one.
(defgeneric compute-child-indentations (wad client))

;;; By default, we do nothing.
(defmethod compute-child-indentations (wad client)
  (declare (ignore wad client))
  nil)

;;; This function is called when we have a simple form, and we have no
;;; particular information about the operator, i.e. either the
;;; operator is a LAMBDA expression, a symbol that is know to be the
;;; name of a function, but the lambda list of of that function is
;;; very simple, or a symbol that we have no information about.
(defun indent-default-function-call (wad client)
  (let ((arguments (rest (children wad))))
    (unless (null arguments)
      (let* ((first-child (first arguments))
             (first-child-start-line (start-line first-child))
             (first-child-start-column (start-column first-child)))
        (unless (zerop first-child-start-line)
          ;; If the start line of the first child is 0, that means
          ;; that the first child is positioned on the same line as
          ;; the operator, so we do not modify its indentation.  If it
          ;; is different from 0, then we set the indentation to be 2
          ;; columns with respect to the start column of the wad.
          (setf (indentation first-child) (+ 2 (start-column wad))))
        (loop for child in (rest arguments)
              unless (zerop (start-line child))
                ;; If the start line of the child is 0, that means
                ;; that this child is positioned on the same line as
                ;; the previous child, so we do not modify its
                ;; indentation.  If it is different from 0, then we
                ;; set the indentation so that it aligns with the
                ;; start column of the first child of the wad.
                do (setf (indentation child) first-child-start-column)
              do (compute-child-indentations child client))))))

;;; We define a form to be SIMPLE if and only if it is a proper list,
;;; and if the elements of the list correspond exactly to the
;;; expression wads of the children of WAD in the same order.

(defun children-correspond-p (form wad-children)
  (if (null form)
      (or (null wad-children)
          (and (not (typep (car wad-children)
                           'expression-wad))
               (children-correspond-p form (cdr wad-children))))
      (and (not (null wad-children))
           (if (typep (car wad-children) 'expression-wad)
               (and (eq (car form)
                        (expression (car wad-children)))
                    (children-correspond-p (cdr form)
                                           (cdr wad-children)))
               (children-correspond-p form (cdr wad-children))))))

(defun proper-list-p (object)
  (not (null (ignore-errors (list-length object)))))

(defun simple-form-p (wad)
  (let ((expression (expression wad)))
    (and (consp expression)
         (proper-list-p expression)
         (children-correspond-p expression (children wad)))))

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

;;; The generic function called in order to compute the relative
;;; indentation of the children of WAD.  Each method defines an EQL
;;; specializer on the PAWN parameter.  Client code should specialize
;;; on its own CLIENT class.
(defgeneric compute-sub-form-indentations (wad pawn client))

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

(defmethod compute-child-indentations ((wad expression-wad) client)
  (if (simple-form-p wad)
      (let ((first-child (first (children wad))))
        (if (and (typep first-child 'expression-wad)
                 (typep (expression first-child) 'symbol-token))
            (let* ((token (expression first-child))
                   (pawn (find-pawn (package-name token)
                                    (name token))))
              (if (null pawn)
                  (indent-default-function-call wad client)
                  (compute-sub-form-indentations wad pawn client)))
            (indent-default-function-call wad client)))
      (indent-default-function-call wad client)))

(defun indent-body (column body-wads client)
  (loop for wad in body-wads
        unless (zerop (start-line wad))
          do (setf (indentation wad) column)
        do (compute-child-indentations wad client)))

(defun indent-up-to-and-including-expression (column wads client)
  (loop for remaining on wads
        for wad = (first remaining)
        unless (zerop (start-line wad))
          do (setf (indentation wad) column)
        when (typep wad 'expression-wad)
          do (compute-binding-indentations wad client)
             (loop-finish)
        finally (return (rest remaining))))

(defun wad-represents-symbol-p (wad symbol)
  (and (typep wad 'expression-wad)
       (let ((expression (expression wad)))
         (and (typep expression 'symbol-token)
              (equal (package-name expression)
                     (cl:package-name (symbol-package symbol)))
              (equal (name expression)
                     (cl:symbol-name symbol))))))

(defun first-child-wad-represents-symbol-p (wad symbol)
  (and (typep wad 'expression-wad)
       (let ((children (children wad)))
         (and (consp children)
              (wad-represents-symbol-p (first children) symbol)))))

(defun indent-list (wad element-indent-function)
  (let ((children (children wad)))
    (unless (null children)
      (destructuring-bind (first . rest) children
        (when (typep first 'expression-wad)
          (funcall element-indent-function first))
        (loop for child in rest
              unless (zerop (start-line child))
                do (setf (indentation child)
                         (start-column first ))
              when (typep child 'expression-wad)
                do (funcall element-indent-function child))))))

(defun indent-simple-list (wad)
  (let ((children (children wad)))
    (unless (null children)
      (loop for child in (rest children)
            unless (zerop (start-line child))
              do (setf (indentation child)
                       (start-column (first children)))))))

;;; Many standard forms have a distinguished expression as their first
;;; argument.  If the distinguished expression is on the same line as
;;; the operator, then no particular indentation is imposed, but if it
;;; is on a line by itself. it is indented DELTA-INDENTATION compared
;;; to the START-COLUMN of WAD.  The distinguished expression may be
;;; preceded by an arbitrary number of NON-EXPRESSION-WADs.  The first
;;; wad determines the indentation, whether it is an expression wad or
;;; not, so that all the NON-EXPRESSION-WADs and the distinguished
;;; expression wad are aligned.  This function returns a list of the
;;; wads that remain after the distinguished expression wad.  If there
;;; is no expression wad among the children, or if there are no more
;;; wads after the first one, then the empty list is returned.  The
;;; parameter DISTINGUISHED-EXPRESSION-INDENT-FUNCTION is a function
;;; that is applied to the distinguished expression.  We need this
;;; parameter, because there is great variation as to the nature of
;;; the distinguished expression, and how it should be indented.
(defun compute-distinguished-indentation
    (arguments indentation indent-function)
  (if (null arguments)
      '()
      (destructuring-bind (first . remaining) arguments
        (let ((distinguished-indentation (start-column first)))
          (unless (zerop (start-line first))
            (setf (indentation first) indentation))
          (if (typep first 'expression-wad)
              (progn (funcall indent-function first)
                     remaining)
              (loop for (argument . rest) on remaining
                    unless (zerop (start-line argument))
                      do (setf (indentation argument)
                               distinguished-indentation)
                    when (typep argument 'expression-wad)
                      do (funcall indent-function argument)
                         (loop-finish)
                    finally (return rest)))))))

;;; Many Common Lisp operators have a first argument that is
;;; distinguished in some way, followed by one or more expression that
;;; are part of a sequence of similar expressions.  If the
;;; distinguished argument starts on the same line as the operator, no
;;; indentation is imposed on it.  If it starts on a separate line, it
;;; is indented by 4 positions, compared to the parent expression.
;;; The remaining expressions are indented by 2 position, compared to
;;; the parent expression.  This computation needs two parameters,
;;; namely a function that determines how the distinguished argument
;;; is to be recursively indented, and a function that determines how
;;; the remaining expressions are to be indented.
(defun compute-indentation-single-distinguished
    (wad compute-distinguished-indentation compute-remaining-indentation)
  (let ((remaining-wads (compute-distinguished-indentation
                         (rest (children wad))
                         (+ (start-column wad) 4)
                         compute-distinguished-indentation)))
    (funcall compute-remaining-indentation
             (+ (start-column wad) 2)
             remaining-wads)))

(defun indent-line (analyzer line-number)
  (declare (ignore analyzer))
  (let* ((window (esa:current-window))
         (view (clim:stream-default-view window))
         (history (clim-base::output-history view))
         (wad (find-wad-beginning-line
               history line-number)))
    (unless (or (null wad) (null (indentation wad)))
      (clim-base::with-current-cursor (cursor)
        (base:beginning-of-line cursor)
        (let ((start-column (start-column wad))
              (indentation (indentation wad)))
          (loop repeat (- start-column indentation)
                do (base:delete-item cursor))
          (loop repeat (- indentation start-column)
                do (base:insert-item cursor #\Space))
          (loop while (eql (base:item-after-cursor cursor)
                           #\Space)
                do (base:forward-item cursor)))))))

;;; Given a list of wads, extract all the wads up to and including the
;;; first expression wad.  Return those wads as a list in reverse
;;; order, i.e. the expression wad is the first wad on the list.  As a
;;; second return argument, return the remaining wads in the original
;;; list.  If there is no expression wad in the original list of wads,
;;; then the first wad in the first list returned will not be an
;;; expression wad and the second list returned will be empty.  If the
;;; original list of wads is empty, two empty lists will be returned.
(defun extract-up-to-expression-wad (wads)
  (let ((first-result '())
        (second-result wads))
    (loop until (null second-result)
          do (push (pop second-result) first-result)
          until (typep (first first-result) 'expression-wad)
          finally (return (values first-result second-result)))))

;;; Take a list of wads and split it into wad groups, and return a
;;; list of those wad groups.  A wad group is a list of wads.  Each
;;; wad group, except possibly the last one, contains exactly one
;;; expression wad as its last element.  The last wad group may
;;; contain only non-expression wads.
(defun split-wads (wads)
  (loop with remaining = wads
        until (null remaining)
        collect (loop for first = (pop remaining)
                      collect first
                      until (or (null remaining)
                                (typep first 'expression-wad)))))

;;; Take a list of wads.  Use the column of the first one to align all
;;; the remaining ones that are on the beginning of a line.
(defun align-with-first (wads)
  (loop with indentation = (start-column (first wads))
        for wad in (rest wads)
        unless (zerop (start-line wad))
          do (setf (indentation wad) indentation)))

;;; Take a list of wads.  If the first wad is not first on a line, then
;;; then indent the others like the first one.  If the first wad IS first
;;; on a line, then indent every wad according to INDENTATION.
(defun align-or-indent (wads indentation)
  (if (zerop (start-line (first wads)))
      (align-with-first wads)
      (loop for wad in wads
            unless (zerop (start-line wad))
              do (setf (indentation wad) indentation))))
