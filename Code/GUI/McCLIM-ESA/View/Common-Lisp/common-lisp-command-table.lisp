(cl:in-package #:second-climacs-clim-view-common-lisp)

(clim:define-command-table common-lisp-table
  :inherit-from
  (clim-base:global-table
   clim-base:ascii-insert-table
   clim-base:delete-table
   clim-base:motion-table))

(clim:define-command
    (com-up-expression :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (cl-syntax:up-expression cache cursor)))

(esa:set-key `(com-up-expression)
             'common-lisp-table
             '((#\u :meta :control)))

(clim:define-command
    (com-forward-expression :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (cl-syntax:forward-expression cache cursor)))

(esa:set-key `(com-forward-expression)
             'common-lisp-table
             '((#\f :meta :control)))

(clim:define-command
    (com-backward-expression :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (cl-syntax:backward-expression cache cursor)))

(esa:set-key `(com-backward-expression)
             'common-lisp-table
             '((#\b :meta :control)))

(clim:define-command
    (com-mark-expression :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (cl-syntax:mark-expression cache cursor)))

(esa:set-key `(com-mark-expression)
             'common-lisp-table
             '((#\Space :meta :control)))

(clim:define-command
    (com-exchange-expressions :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (cl-syntax:exchange-expressions cache cursor)))

(esa:set-key `(com-exchange-expressions)
             'common-lisp-table
             '((#\t :meta :control)))

(clim:define-command
    (com-beginning-of-top-level-expression
     :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (cl-syntax:beginning-of-top-level-expression cache cursor)))

(esa:set-key `(com-beginning-of-top-level-expression)
             'common-lisp-table
             '((#\a :meta :control)))

(clim:define-command
    (com-end-of-top-level-expression
     :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (cl-syntax:end-of-top-level-expression cache cursor)))

(esa:set-key `(com-end-of-top-level-expression)
             'common-lisp-table
             '((#\e :meta :control)))

(clim:define-command
    (com-fill-paragraph
     :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (cl-syntax:fill-paragraph cache cursor)))

(esa:set-key `(com-fill-paragraph)
             'common-lisp-table
             '((#\q :meta)))

(clim:define-command
    (com-print-wad-tree :name t :command-table common-lisp-table)
    ()
  (with-current-cursor-and-cache (cursor cache)
    (declare (ignore cursor))
    (let ((wad (cl-syntax:first-top-level-wad cache)))
      (terpri *trace-output*)
      (cl-syntax:print-wad-tree wad *trace-output*))))

(esa:set-key `(com-print-wad-tree)
             'common-lisp-table
             '((#\w :meta :control)))
