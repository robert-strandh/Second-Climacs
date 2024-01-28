(cl:in-package #:second-climacs-clim-view-common-lisp)

(clim:define-command-table common-lisp-table
  :inherit-from (clim-base:global-table
                 clim-base:ascii-insert-table
                 clim-base:delete-table
                 clim-base:motion-table
                 clim-base::transform-table))

#+no (clim:define-command
    (com-beginning-of-top-level-expression
     :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (cl-syntax:beginning-of-top-level-expression cache cursor)))
#+no (esa:set-key `(com-beginning-of-top-level-expression)
             'common-lisp-table
             '((#\a :meta :control)))

#+no (clim:define-command
    (com-end-of-top-level-expression
     :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (cl-syntax:end-of-top-level-expression cache cursor)))
#+no (esa:set-key `(com-end-of-top-level-expression)
             'common-lisp-table
             '((#\e :meta :control)))

(clim:define-command
    (com-fill-paragraph :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (cl-syntax:fill-paragraph cache cursor)))
(esa:set-key `(com-fill-paragraph) 'common-lisp-table '((#\q :meta)))

(clim:define-command
    (com-print-wad-tree :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (declare (ignore cursor))
    (let ((wad (cl-syntax:first-top-level-wad cache)))
      (terpri *trace-output*)
      (if (null wad)
          (format *trace-output* "--no wads--~%")
          (cl-syntax:print-wad-tree wad *trace-output*)))))
(esa:set-key `(com-print-wad-tree)
             'common-lisp-table
             '((#\w :meta :control)))
