(defsystem "second-climacs-syntax-common-lisp-indentation"
  :depends-on ("second-climacs-syntax-common-lisp-base")
  :serial t
  :components ((:file "indentation-unit")
               (:file "indentation-support")
               (:file "type-specifier")
               (:file "declare")
               (:file "form")
               (:file "let-and-letstar")
               (:file "setf-and-setq")
               (:file "eval-when")
               (:file "prog1-etc")
               (:file "prog2")
               (:file "block-etc")
               (:file "locally")
               (:file "tagbody")
               (:file "multiple-value-setq")
               (:file "lambda-list")
               (:file "flet-labels-macrolet")
               (:file "the")
               (:file "destructuring-bind-etc")
               (:file "defun")
               (:file "make-instance-etc")
               (:file "defclass")
               (:file "defmethod")
               (:file "case")
               (:file "typecase")
               (:file "cond")
               (:file "assert")))
