(defsystem "second-climacs-clim"
  :depends-on ("second-climacs-clim-base"
               "second-climacs-clim-fundamental-view"
               "second-climacs-clim-common-lisp-view"
               "esclados-info-pane")
  :serial t
  :components ((:file "gui")
               (:file "io"))
  ;; Executable
  :build-operation asdf:program-op
  :build-pathname  "second-climacs"
  :entry-point     "second-climacs-clim-base:climacs")
