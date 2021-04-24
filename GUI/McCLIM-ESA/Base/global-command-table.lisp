(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table global-table
  :inherit-from
  (esa:global-esa-table
   esa-io:esa-io-table))

(clim:define-command (com-inspect :name t :command-table global-table) ()
  (clouseau:inspect clim:*application-frame* :new-process t))

(clim:define-command (com-profile-reset :name t :command-table global-table) ()
  (sb-profile:reset))

;; (clim:define-command
;;     (com-idel :name t :command-table delete-table)
;;     ()
;;   (with-current-cursor-and-view (cursor view)
;;     (sb-profile:reset)
;;     (loop repeat 10000
;;           do (climacs2-base:insert-item cursor #\()
;;              (second-climacs-clim-view-common-lisp::update-cache
;;               nil nil
;;               (second-climacs-base:analyzer view))
;;              (climacs2-base:erase-item cursor)
;;              (second-climacs-clim-view-common-lisp::update-cache
;;               nil nil
;;               (second-climacs-base:analyzer view)))))

;; (clim:define-command
;;     (com-iquo :name t :command-table delete-table)
;;     ()
;;   (with-current-cursor-and-view (cursor view)
;;     (sb-profile:reset)
;;     (loop repeat 100
;;           do (climacs2-base:insert-item cursor #\")
;;              (second-climacs-clim-view-common-lisp::update-cache
;;               nil nil
;;               (second-climacs-base:analyzer view))
;;              (climacs2-base:erase-item cursor)
;;              (second-climacs-clim-view-common-lisp::update-cache
;;               nil nil
;;               (second-climacs-base:analyzer view)))))

;; (clim:define-command
;;     (com-idx :name t :command-table delete-table)
;;     ()
;;   (with-current-cursor-and-view (cursor view)
;;     (sb-profile:reset)
;;     (loop repeat 10000
;;           do (climacs2-base:insert-item cursor #\x)
;;              (second-climacs-clim-view-common-lisp::update-cache
;;               nil nil
;;               (second-climacs-base:analyzer view))
;;              (climacs2-base:erase-item cursor)
;;              (second-climacs-clim-view-common-lisp::update-cache
;;               nil nil
;;               (second-climacs-base:analyzer view)))))
