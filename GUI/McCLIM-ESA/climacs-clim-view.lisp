(cl:in-package #:climacs-esa-gui)

;;; When a visible window is updated, we need to know what is on
;;; display in that window.  This information can be read and written
;;; by the STREAM-DEFAULT-VIEW accessor for CLIM stream panes.  But
;;; Climacs view classes are independent of the underlying GUI
;;; library, so we can't have Climacs view classes be subclasses of
;;; the CLIM:VIEW class.
;;;
;;; We solve this problem by defining a class CLIMACS-CLIM-VIEW which
;;; is a subclass of CLIM:VIEW and that contains a slot holding an
;;; instance of the Climacs view.

(defclass climacs-clim-view (clim:view)
  ((%climacs-view :initarg :climacs-view :reader climacs-view)))
