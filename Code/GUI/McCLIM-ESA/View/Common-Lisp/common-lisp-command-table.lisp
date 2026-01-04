(cl:in-package #:second-climacs-clim-view-common-lisp)

(clim:define-command-table common-lisp-table
  :inherit-from (clim-base::paredit-table
                 clim-base::transform-table
                 clim-base::search-table
                 clim-base:delete-table
                 clim-base:motion-table
                 clim-base:ascii-insert-table
                 clim-base:global-table))

(clim:define-command
    (com-fill-paragraph :name t :command-table common-lisp-table)
    ()
  (let* ((buffer   (clim-base::current-buffer))
         (view     (clim-base::current-view))
         (analyzer (base:analyzer view)))
    (edit:perform buffer 'cl-syntax:fill-paragraph analyzer)))
(tbl:set-key `(com-fill-paragraph) 'common-lisp-table '((#\q :meta)))

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
(tbl:set-key `(com-print-wad-tree)
             'common-lisp-table
             '((#\w :meta :control)))
