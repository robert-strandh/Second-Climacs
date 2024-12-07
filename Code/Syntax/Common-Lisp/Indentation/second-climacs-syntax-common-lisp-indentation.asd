(defsystem "second-climacs-syntax-common-lisp-indentation"
  :depends-on ("second-climacs-syntax-common-lisp")
  :serial t
  :components ((:file "indentation-unit")
               (:file "indentation-support")
               (:file "type-specifier")
               (:file "form")
               (:file "declare")
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
               (:file "throw")
               (:file "destructuring-bind-etc")
               (:file "defun")
               (:file "make-instance-etc")
               (:file "defclass")
               (:file "defmethod")
               (:file "case")
               (:file "typecase")
               (:file "cond")
               (:file "assert")
               (:file "check-type")
               (:file "defgeneric")
               (:file "do-dostar")
               (:file "dolist-dotimes")
               (:file "with-input-from-string")
               (:file "defconstant")
               (:file "define-modify-macro")
               (:file "defsetf")
               (:file "with-slots")
               (:file "defpackage")
               (:file "prog-progstar")
               (:file "handler-case")
               (:file "handler-bind")
               (:file "restart-case")
               (:file "print-unreadable-object")))
