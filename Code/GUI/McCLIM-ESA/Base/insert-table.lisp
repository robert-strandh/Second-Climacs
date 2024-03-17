(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table ascii-insert-table)

(clim:define-command (com-insert-item :name          t
                                      :command-table ascii-insert-table)
    ((item 'character))
  (let ((buffer (current-buffer)))
    (edit:perform buffer 'edit::insert-item item)
    ; (setf (esa-buffer:needs-saving buffer) t) ; TODO too annoying for testing
    ))

(loop for i from 32 to 126
      for char = (code-char i)
      do (bind-key 'ascii-insert-table `(,char) 'com-insert-item char))
(bind-key 'ascii-insert-table '(#\Return)     'com-insert-item #\Newline)
(bind-key 'ascii-insert-table '(#\j :control) 'com-insert-item #\Newline)

(define-buffer-command (com-open-line ascii-insert-table) ()
  (edit:perform buffer 'edit::insert-item #\Newline)
  (edit:perform buffer 'edit:move edit:item :backward))
(bind-key 'ascii-insert-table '(#\o :control) 'com-open-line)

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
