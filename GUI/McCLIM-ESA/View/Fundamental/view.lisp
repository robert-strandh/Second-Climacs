(cl:in-package #:climacs-clim-view-fundamental)

(stealth-mixin:define-stealth-mixin
    output-history
    (clim:output-record clim:stream-output-history-mixin)
  climacs-syntax-fundamental:analyzer
  ((%parent :initarg :parent :accessor clim:output-record-parent)))

(defclass fundamental-view (second-climacs-clim-base:climacs-clim-view)
  ())

(defmethod second-climacs-clim-base:make-climacs-clim-view
    ((view climacs-syntax-fundamental:view))
  (let ((analyzer (climacs2-base:analyzer view)))
    (make-instance 'fundamental-view
      :output-history analyzer
      :climacs-view view)))

(defgeneric draw-line (pane analyzer contents line-number))

(defmethod draw-line (pane (analyzer output-history) contents line-number)
  (let* ((text-style (clim:medium-text-style pane))
         (text-height (clim:text-style-height text-style pane))
         (text-width (clim:text-style-width text-style pane))
         (text-ascent (clim:text-style-ascent text-style pane))
         (y (+ text-ascent (* line-number text-height)))
         (clim-view (clim:stream-default-view pane))
         (climacs-view (second-climacs-clim-base:climacs-view clim-view))
         (cursor (climacs2-base:cursor climacs-view))
         (cursor-column-number (cluffer:cursor-position cursor)))
    (if (= (cluffer:line-number cursor) line-number)
        (cond ((zerop cursor-column-number)
               (clim:draw-rectangle* pane 1 (- y text-height) 4 y
                                     :ink clim:+blue+)
               (unless (zerop (length contents))
                 (clim:draw-text* pane contents 5 y)))
              ((= cursor-column-number (length contents))
               (unless (zerop (length contents))
                 (clim:draw-text* pane contents 0 y))
               (let ((cursor-x (* (length contents) text-width)))
                 (clim:draw-rectangle* pane
                                       (1+ cursor-x) (- y text-height)
                                       (+ cursor-x 4) y
                                       :ink clim:+blue+)))
              (t
               (clim:draw-text* pane contents 0 y
                                :start 0
                                :end cursor-column-number)
               (let ((cursor-x (* cursor-column-number text-width)))
                 (clim:draw-rectangle* pane
                                       (1+ cursor-x) (- y text-height)
                                       (+ cursor-x 4) y
                                       :ink clim:+blue+)
                 (clim:draw-text* pane contents (+ cursor-x 5) y
                                  :start cursor-column-number))))
        (clim:draw-text* pane contents 0 y))))
