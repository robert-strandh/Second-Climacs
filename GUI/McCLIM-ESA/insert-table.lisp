(cl:in-package #:climacs-esa-gui)

(clim:define-command-table ascii-insert-table)

(clim:define-command
    (com-insert-item :name t :command-table ascii-insert-table)
    ((item t))
  (with-current-cursor (cursor)
    (climacs2-base:insert-item cursor item)))

(loop for i from 32 to 126
      for char = (code-char i)
      do (esa:set-key `(com-insert-item ,char)
                      'ascii-insert-table
                      `((,char))))

(esa:set-key `(com-insert-item #\Newline)
             'ascii-insert-table
             '((#\Newline)))

(clim:define-command
    (com-insert-file :name t :command-table ascii-insert-table)
    ((filepath 'pathname
               :prompt "Insert File: "
               :prompt-mode :raw
;;               :default (esa-io::directory-of-current-buffer)
               :default-type 'clim:pathname
;;               :insert-default t))
               ))
  (with-current-cursor (cursor)
    (with-open-file (stream filepath :direction :input)
      (climacs2-base:fill-buffer-from-stream cursor stream))))

(esa:set-key `(com-insert-file ,clim:*unsupplied-argument-marker*)
             'ascii-insert-table '((#\x :control) (#\i)))
