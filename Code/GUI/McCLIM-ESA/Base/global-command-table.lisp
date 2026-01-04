(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table global-table
  :inherit-from (key:global-table io:io-table debug-table))

;;; Font size commands

(clim:define-command (com-set-font-size :name t :command-table global-table)
    ((size '(or integer (member :smaller :larger :normal)))
     &key (window 't :default (frame:current-window)))
  ;; TODO: this should be tied into a configuration system, of course
  ;; FIXME: This is so that the command does not signal an error on
  ;; upstream McCLIM.
  (let ((set-text-style (fdefinition '(setf clim:pane-text-style))))
    (unless (null set-text-style)
      (funcall set-text-style
               (clim:merge-text-styles
                (clim:make-text-style nil nil size)
                (clim:pane-text-style window))
               window)))
  ;; HACK: not applied otherwise
  (let ((medium (clim:sheet-medium window)))
    (setf (clim:medium-text-style medium) (clim:medium-default-text-style medium))))

(bind-key 'global-table '(#\+ :control) 'com-set-font-size :larger)
(bind-key 'global-table '(#\- :control) 'com-set-font-size :smaller)
(bind-key 'global-table '(#\0 :control) 'com-set-font-size :normal)

(clim:define-gesture-name :increase-font-size :pointer-scroll
  (:wheel-up :control))

(clim:define-presentation-to-command-translator blank-area->increase-font-size
    (clim:blank-area com-set-font-size global-table
     :gesture :increase-font-size)
    (object)
  '(:larger))

(clim:define-gesture-name :decrease-font-size :pointer-scroll
  (:wheel-down :control))

(clim:define-presentation-to-command-translator blank-area->decrease-font-size
    (clim:blank-area com-set-font-size global-table
     :gesture :decrease-font-size)
    (object)
  '(:smaller))
