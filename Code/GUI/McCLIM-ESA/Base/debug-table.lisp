(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table debug-table)

;;; Printing commands

(defun print-expression-tree (cursor &key (stream *trace-output*))
  (let ((toplevel-expression (edit.exp:innermost-expression-containing-cursor
                              cursor :ast)))
    (fresh-line stream)
    (utilities.print-tree:print-tree
     stream toplevel-expression
     (utilities.print-tree:make-node-printer
      (lambda (stream depth node)
        (declare (ignore depth))
        (princ node stream))
      nil #'edit.exp:children))))

(define-buffer-command (com-print-expression-tree debug-table)
    ()
  (edit:perform buffer 'print-expression-tree))

;;; Inspection commands

(clim:define-command (com-inspect :name t :command-table debug-table) ()
  (clouseau:inspect clim:*application-frame* :new-process t))

(clim:define-command (com-inspect-buffer :name t :command-table debug-table)
    ()
  (clouseau:inspect (current-buffer) :new-process t))

(clim:define-command (com-inspect-analyzer :name t :command-table debug-table)
    ()
  (let* ((view     (current-view))
         (analyzer (base:analyzer view)))
    (clouseau:inspect analyzer :new-process t)))

;;; Test commands

(clim:define-command (com-random-edits :name t :command-table debug-table)
    ((count 'integer :default 10))
  (loop :with buffer = (current-buffer)
        :with point = (edit:point buffer)
        :repeat count
        :for line-number = (random (cluffer:line-count buffer))
        :for line = (cluffer:find-line buffer line-number)
        :do (edit:move-cursor-to-line
             point line (random (1+ (cluffer:item-count line))))
            (case (random 5)
              (0         (cluffer:split-line point))
              ((1 2 3 4) (cluffer:insert-item point #\a)))))
