(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table ascii-insert-table)

(clim:define-command
    (com-insert-item :name t :command-table ascii-insert-table)
    ((item t))
  (with-current-cursor (cursor)
    (base:insert-item cursor item)
    (setf (esa-buffer:needs-saving (base:buffer cursor)) t)))

(loop for i from 32 to 126
      for char = (code-char i)
      do (esa:set-key `(com-insert-item ,char)
                      'ascii-insert-table
                      `((,char))))

(esa:set-key `(com-insert-item #\Newline)
             'ascii-insert-table
             '((#\Return)))

(clim:define-command
    (com-open-line :name t :command-table ascii-insert-table)
    ()
  (with-current-cursor (cursor)
    (base:insert-item cursor #\Newline)
    (base:backward-item cursor)))

(esa:set-key `(com-open-line)
             'ascii-insert-table
             '((#\o :control)))

(clim:define-command
    (com-insert-file :name t :command-table ascii-insert-table)
    ((filepath 'pathname
               :prompt "Insert File: "
               :prompt-mode :raw
               :default (esa-io::directory-of-current-buffer)
               :default-type 'clim:pathname
               :insert-default t))
  (with-current-cursor (cursor)
    (with-open-file (stream filepath :direction :input)
      (base:fill-buffer-from-stream cursor stream))))

(esa:set-key `(com-insert-file ,clim:*unsupplied-argument-marker*)
             'ascii-insert-table '((#\x :control) (#\i)))
