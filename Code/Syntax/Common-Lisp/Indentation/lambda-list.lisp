(cl:in-package #:second-climacs-syntax-common-lisp)

(defun wad-represents-lambda-list-keyword-p (wad)
  (loop for keyword in lambda-list-keywords
        thereis (wad-represents-symbol-p wad keyword)))

(define-indentation-automaton compute-lambda-list-indentations
  (macrolet ((go-accordingly (lambda-list-keyword)
               `(when (wad-represents-symbol-p
                       current-wad ',lambda-list-keyword)
                  (next)
                  (if (wad-represents-lambda-list-keyword-p
                       current-wad)
                      (go lambda-list-keyword)
                      (go ,lambda-list-keyword)))))
    (tagbody
       (next)
     could-be-anything
       (maybe-assign-indentation 1 1)
       ;; As long as the current wad does not represent a
       ;; lambda-list keywod symbol, we consider it to be a
       ;; required parameter.  Of course, that means that a
       ;; required parameter can be just about any expression, but
       ;; it is not the role of the indentation logic to detect
       ;; such issues.
       (unless (wad-represents-lambda-list-keyword-p current-wad)
         (next)
         (go could-be-anything))
     lambda-list-keyword
       ;; Come here when we have a lambda-list keyword.  The
       ;; indentation code should not care too much about the
       ;; order between the lambda-list keywords.  But it should
       ;; recognize the kind of keyword so that it can indent the
       ;; keyword parameter accordingly.  So what we do is that we
       ;; recognize that we are in a particular group of
       ;; parameters according to the lambda-list keyword that
       ;; precedes it, but we do not check the order nor the
       ;; number of such groups.
       (maybe-assign-indentation 3 1)
       (go-accordingly &optional)
       (go-accordingly &rest)
       (go-accordingly &body)
       (go-accordingly &whole)
       (go-accordingly &environment)
       (go-accordingly &key)
       (go-accordingly &allow-other-keys)
       (go-accordingly &aux)
     unknown
       ;; Come here when we have an unknown lambda-list keyword.
       ;; We do not process the possible parameters that might
       ;; follow, because we don't know their nature.  But we
       ;; indent them as members of the group.
       (if (wad-represents-lambda-list-keyword-p current-wad)
           (go lambda-list-keyword)
           (progn (maybe-assign-indentation 3 3)
                  (next)
                  (go unknown)))
     &optional
       ;; Come here when the current wad ought to represent an
       ;; optional parameter.  We try to determine whether we have
       ;; an optional form to indent.
       (maybe-assign-indentation 3 3)
       (when (typep current-wad 'ip:expression-wad)
         (let* ((expression (ip:expression current-wad))
                (children (ip:children current-wad)))
           (when (consp expression)
             ;; We try to find an expression wad that
             ;; follows the first child
             (let ((init-form-wad
                     (find-if (lambda (x)
                                (typep x 'ip:expression-wad))
                              (rest children))))
               (unless (null init-form-wad)
                 (compute-form-indentation init-form-wad nil client))))))
       (next)
       (when (wad-represents-lambda-list-keyword-p current-wad)
         (go lambda-list-keyword))
       (go &optional)
     &rest
     &body
       ;; Come here when the current wad ought to represent a
       ;; &REST or a &BODY parameter.  The thing is that in a
       ;; macro lambda list or a destructuring lambda list, such a
       ;; parameter can be another lambda list.
       (maybe-assign-indentation 3 3)
       (compute-lambda-list-indentation current-wad client)
       (next)
       (when (wad-represents-lambda-list-keyword-p current-wad)
         (go lambda-list-keyword))
       (go &rest)
     &whole
     &environment
       ;; Come here when the current wad ought to represent a
       ;; &WHOLE or an &ENVIRONMENT parameter.  These lambda-list
       ;; keywords should be followed by a single variable, but it
       ;; is not the purpose of the indentation code to varify
       ;; such things, so we just act as if they can be followed
       ;; by more than one variable.
       (maybe-assign-indentation 3 3)
       (next)
       (when (wad-represents-lambda-list-keyword-p current-wad)
         (go lambda-list-keyword))
       (go &whole)
     &key
       ;; Come here when the current wad ought to represent a &KEY
       ;; parameter.  We are mainly interested in whether there is
       ;; an INIT-FORM that should have its indentation computed.
       (maybe-assign-indentation 3 3)
       (when (typep current-wad 'ip:expression-wad)
         (let ((expression (ip:expression current-wad)))
           (when (and (consp expression)
                      (consp (rest expression)))
             (let ((pos (position-if
                         (lambda (x) (typep x 'ip:expression-wad))
                         (ip:children current-wad))))
               (unless (null pos)
                 (let ((init-form-wad
                         (find-if
                          (lambda (x) (typep x 'ip:expression-wad))
                          (ip:children current-wad)
                          :start (1+ pos))))
                   (unless (null init-form-wad)
                     (compute-form-indentation init-form-wad nil client))))))))
       (next)
       (when (wad-represents-lambda-list-keyword-p current-wad)
         (go lambda-list-keyword))
       (go &key)
     &allow-other-keys
       ;; Come here when the current wad represents something
       ;; following &ALLOW-OTHER-KEYS.
       (when (wad-represents-lambda-list-keyword-p current-wad)
         (go lambda-list-keyword))
       ;; There shouldn't be anything here, but there is, so we indent
       ;; it.
       (maybe-assign-indentation 3 3)
       (next)
       (go &allow-other-keys)
     &aux
       ;; Come here when we should have an AUX "parameter".
       (maybe-assign-indentation 3 3)
       (compute-binding-indentation current-wad client)
       (next)
       (go &aux))))

(defun compute-lambda-list-indentation (wad client)
  (compute-and-assign-indentations
   client wad compute-lambda-list-indentations))
