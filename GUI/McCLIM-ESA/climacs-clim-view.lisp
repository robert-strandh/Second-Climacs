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
;;;
;;; When a Climacs view is no longer on display in a pane, we simply
;;; discard the instance of this class referred to by the pane by
;;; setting the STREAM-DEFAULT-VIEW to +TEXTURAL-VIEW+.  In order for
;;; a Climacs view to be displayed in some pane, we allocate a new
;;; instance of this class.

(defclass climacs-clim-view (clim:view)
  ((%climacs-view :initarg :climacs-view :accessor climacs-view)
   ;; This slot contains the timestamp corresponding to the
   ;; last time this view was updated from the Cluffer buffer.
   (%timestamp :initform nil :accessor timestamp)
   ;; This slot contains a flexichain of CONS cells.  The CAR of each
   ;; cell is a Cluffer line object.  The CDR is a vector containing
   ;; the contents of the Cluffer line object as it was when the
   ;; contents was asked for.
   (%lines :initform (make-instance 'flexichain:standard-flexichain)
	   :accessor lines)))
