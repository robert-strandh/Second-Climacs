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

(defgeneric analyzer (view))

(defgeneric cursor (view))

(defclass view ()
  ((%analyzer :initarg :analyzer :reader analyzer)
   (%cursor :initarg :cursor :reader cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function UPDATE-VIEW-FROM-ANALYZER.
;;;
;;; This generic function is called by the ordinary function
;;; UPDATE-VIEW, passing the view and the analyzer of the view as
;;; arguments.  Client code may define primary or auxiliary methods on
;;; this generic function, subject to the usual restrictions about
;;; subclassing.
;;;
;;; When this function is called, it is assumed that the analyzer has
;;; already been updated.  Primary and auxiliary methods on this
;;; generic function may assume that this is the case.
;;;
;;; A buffer can play the role of the analyzer.  This situation occurs
;;; when the analyzer is a NULL-ANALYZER, in which case, this generic
;;; function is called recursively with the buffer of the analyzer in
;;; place of the analyzer.

(defgeneric update-view-from-analyzer (view analyzer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function UPDATE-VIEW.
;;;
;;; This function is called in order to update the view passed as an
;;; argument.  All it does is to trampoline to
;;; UPDATE-VIEW-FROM-ANALYZER, passing it the view and the analyzer of
;;; the view.

(defun update-view (view)
  (update-view-from-analyzer view (analyzer view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on UPDATE-VIEW-FROM-ANALYZER specialized to NULL-ANALYZER.
;;;
;;; This method calls UPDATE-VIEW-FROM-ANALYZER recursively with the
;;; buffer of the analyzer in place of the analyzer itself.

(defmethod update-view-from-analyzer (view (analyzer null-analyzer))
  (update-view-from-analyzer view (buffer analyzer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function HIDE-VIEW.
;;;
;;; This generic function is called when a view is no longer
;;; associated with a window.  The default primary method does
;;; nothing.  Client code may define auxiliary methods on subclasses
;;; of VIEW.

(defgeneric hide-view (view))

(defmethod hide-view ((view view))
  (declare (ignorable view))
  nil)
