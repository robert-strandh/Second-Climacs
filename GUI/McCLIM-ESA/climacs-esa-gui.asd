(cl:in-package #:asdf-user)

(defsystem :climacs-esa-gui
  :depends-on (:mcclim
	       :cluffer
	       :second-climacs-base
	       :climacs-flexichain-output-history
	       :climacs-syntax-common-lisp
	       :clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "climacs-clim-view")
   (:file "text-pane")
   (:file "fundamental-view")
   (:file "common-lisp-view")
   (:file "with-current-cursor")
   (:file "insert-table")
   (:file "delete-table")
   (:file "motion-table")
   (:file "global-command-table")
   (:file "fundamental-command-table")
   (:file "common-lisp-command-table")
   (:file "view-names")
   (:file "gui")
   (:file "io")))
