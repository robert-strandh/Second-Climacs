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
;;; setting the STREAM-DEFAULT-VIEW to +TEXTUAL-VIEW+.  In order for
;;; a Climacs view to be displayed in some pane, we allocate a new
;;; instance of this class.

(defgeneric climacs-view (clim-view))

(defgeneric (setf climacs-view) (climacs-view clim-view))

(defgeneric timestamp (clim-view))

(defgeneric (setf timestamp) (time-stamp clim-view))

(defgeneric lines (clim-view))

(defgeneric (setf lines) (lines clim-view))

(defclass climacs-clim-view (clim:view)
  ((%climacs-view :initarg :climacs-view :accessor climacs-view)
   ;; This slot contains the output history to be used to
   ;; display the pane that this view is the default-view of.
   (%output-history :initarg :output-history :accessor output-history)
   ;; This slot contains the timestamp corresponding to the
   ;; last time this view was updated from the Cluffer buffer.
   (%timestamp :initform nil :accessor timestamp)
   ;; This slot contains a flexichain of CONS cells.  The CAR of each
   ;; cell is a Cluffer line object.  The CDR is a vector containing
   ;; the contents of the Cluffer line object as it was when the
   ;; contents was asked for.
   (%lines :initform (make-instance 'flexichain:standard-flexichain)
           :accessor lines)))

;;; This function takes an instance of a Climacs view and returns an
;;; instance of the corresponding subclass of CLIMACS-CLIM-VIEW.

(defgeneric make-climacs-clim-view (climacs-view))

;;; This generic function takes an instance of a Climacs view, and
;;; returns a command table to be used for this particular view.

(defgeneric command-table (climacs-view))
