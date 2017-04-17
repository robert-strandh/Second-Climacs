(cl:in-package #:second-climacs-clim-common-lisp-view)

(defun scroll-extent (pane x y)
  (multiple-value-bind (width height)
      (text-style-dimensions pane)
    (clim:scroll-extent pane (* x width) (* y height))))

(defun move-viewport-to-cursor (pane)
  (let* ((clim-view (clim:stream-default-view pane))
         (climacs-view (second-climacs-clim-base:climacs-view clim-view))
         (cursor (climacs2-base:cursor climacs-view))
         (cursor-line-number (cluffer:line-number cursor))
         (cursor-column-number (cluffer:cursor-position cursor)))
    (multiple-value-bind (left top right bottom)
        (viewport-area pane)
      (unless (and (<= top cursor-line-number bottom)
                   (<= left cursor-column-number right))
        (scroll-extent pane
                       (max 0 (- cursor-column-number
                                 (floor (- right left) 2)))
                       (max 0 (- cursor-line-number
                                 (floor (- bottom top) 2))))))))
