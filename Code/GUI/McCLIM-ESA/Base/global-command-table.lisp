(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table global-table
  :inherit-from
  (esa:global-esa-table
   esa-io:esa-io-table))

(clim:define-command (com-inspect :name t :command-table global-table) ()
  (clouseau:inspect clim:*application-frame* :new-process t))

(clim:define-command (com-inspect-buffer :name t :command-table global-table)
    ()
  (clouseau:inspect (current-buffer) :new-process t))

(clim:define-command (com-random-edits :name t :command-table global-table)
    ((count 'integer :default 10))
  (loop :with buffer = (current-buffer)
        :with point = (edit:point buffer)
        :repeat count
        :for line-number = (random (cluffer:line-count buffer))
        :for line = (cluffer:find-line buffer line-number)
        :do (edit:move-cursor-to-line point line (random (1+ (cluffer:item-count line))))
        (case (random 5)
          (0         (cluffer:split-line point))
          ((1 2 3 4) (cluffer:insert-item point #\a)))))
