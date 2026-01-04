(cl:in-package #:second-climacs-clim-base)

(clim:define-presentation-type buffer ()
  :inherit-from t)

(clim:define-presentation-type cursor ()
  :inherit-from t)

(clim:define-presentation-type-abbreviation unit ()
  `((member ,@(edit:all-units))
    :name-key ,(alexandria:compose #'symbol-name #'class-name #'class-of)))

(clim:define-presentation-type-abbreviation direction ()
  '(member :forward :backward))

;;;

(defun bind-key (table key command-name &rest command-arguments)
  (let ((command `(,command-name ,@command-arguments)))
    (tbl:set-key command table (list key))))

(defmacro define-buffer-command ((function-name command-table &key (name t))
                                 (&rest extra-parameters)
                                 &body body)
  (multiple-value-bind (required key)
      (loop :with section = :required
            :for item :in extra-parameters
            :if (eq item '&key)
              :do (setf section :key)
            :else
              :if (eq section :required)
                :collect item :into required
              :else :if (eq section :key)
                :collect item :into key
            :finally (return (values required key)))
    `(clim:define-command (,function-name :command-table ,command-table
                                          :name          ,name)
         (,@required &key ,@key (buffer 'buffer :default (current-buffer)))
       ,@body)))

(defmacro define-command-specialization ((command-table name base-command
                                          &optional keystroke)
                                         &body arguments)
  `(progn
     (clim:define-command (,name :name t :command-table ,command-table)
         ()
       (,base-command ,@arguments))

     ,@(when keystroke
         `((bind-key ',command-table ',keystroke ',name)))))

(defmacro define-command-specializations ((command-table base-command)
                                          &body clauses)
  (flet ((process-clause (clause)
           (destructuring-bind (name (&rest arguments) &optional gesture) clause
             `(define-command-specialization (,command-table ,name ,base-command
                                              ,@(when gesture `(,gesture)))
                ,@arguments))))
    `(progn ,@(mapcar #'process-clause clauses))))

(defmacro define-unit-specializations ((command-table base-command
                                        &key (name-template '(com #:- :unit #:- :direction)))
                                       &body clauses)

  (flet ((process-clause (clause)
           (destructuring-bind (unit modifiers &optional forward-key backward-key) clause
             (labels ((name (direction)
                        (apply #'alexandria:symbolicate
                               (sublis `((:unit . ,unit) (:direction . ,direction))
                                       name-template)))
                      (expand (direction key)
                        `(,(name direction) (:unit ,unit :direction ,direction)
                          ,@(when key `((,key  ,@modifiers))))))
               `(,(expand :forward  forward-key)
                 ,(expand :backward backward-key))))))
    `(define-command-specializations (,command-table ,base-command)
       ,@(alexandria:mappend #'process-clause clauses))))
