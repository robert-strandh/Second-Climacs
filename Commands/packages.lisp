(cl:in-package #:common-lisp-user)

(defpackage #:climacs-commands
  (:use #:common-lisp)
  (:export
   #:emacs-style-command-processor
   #:*command-mappings*
   #:*ascii-insert-mappings*
   #:*latin-1-insert-mappings*
   #:*point*
   ;; FIXME: remove this later when we create command the processors
   ;; based on the type of the view.
   #:make-fundamental-command-processor
   ))

