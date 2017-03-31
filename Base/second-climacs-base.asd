(cl:in-package #:asdf-user)

(defsystem :second-climacs-base
  :depends-on (:cluffer
	       :cluffer-emacs-compatibility)
  :serial t
  :components
  ((:file "packages")
   (:file "buffer")
   (:file "standard-buffer")
   (:file "analyzer")
   (:file "null-analyzer")
   (:file "view")
   (:file "view-class")
   (:file "fundamental-view")
   (:file "window")
   (:file "application")))
