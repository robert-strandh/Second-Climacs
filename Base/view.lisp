(cl:in-package #:second-climacs-base)

;;; A VIEW is an object that determines some aspects about how the
;;; result of an analysis is to be shown to the user.
;;;
;;; Several views can share a single analyzer object.  This situation
;;; occurs when it is desirable to present the same analysis result in
;;; more than one way for different purposes.
;;;
;;; Objects of this type are independent of the interface manager.  If
;;; the interface manager has a class VIEW, as CLIM II does, then an
;;; object of the interface-manager class typically contains a
;;; reference to an object of this Climacs-specific class.
;;;
;;; An instance of this class does not necessarily represent anything
;;; that is currently visible to the end user.  Only a subset of all
;;; the views managed by Climacs are typically visible at a given
;;; point in time.  Only the visible views are kept updated.
;;;
;;; At any point in time, there is a single "current view".  Climacs
;;; commands operate on the current view.  Some of these commands are
;;; handled by the view itself, for example commands for scrolling or
;;; for positioning the cursor.  Some commands are forwarded to the
;;; analyzer and handled by it.  An example of such a command would be
;;; to highlight all symbols in a particular package.  Finally, some
;;; commands are forwarded to the buffer of the analyzer.  Commands
;;; for inserting and deleting text are examples of such commands.

(defclass view ()
  ((%analyzer :initarg :analyzer :reader analyzer)))
