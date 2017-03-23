(cl:in-package #:climacs-clim-view-fundamental)

(stealth-mixin:define-stealth-mixin
    output-history
    (clim:output-record clim:stream-output-history-mixin)
  climacs-syntax-fundamental:cache
  ((%parent :initarg :parent :accessor clim:output-record-parent)))

(defclass fundamental-view (second-climacs-clim-base:climacs-clim-view)
  ())

(defmethod second-climacs-clim-base:make-climacs-clim-view
    ((view climacs-syntax-fundamental:view))
  (let ((analyzer (climacs2-base:analyzer view)))
    (make-instance 'fundamental-view
      :output-history cache
      :climacs-view view)))
