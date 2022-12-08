(cl:in-package #:second-climacs-base)

;;; A WINDOW is an object supplied by the underlying interface
;;; manager.  We do not define a window class here, because we want to
;;; allow the use of existing classes in the interface manager.
;;;
;;; Similarly, an APPLICATION is a representation of the application
;;; that contains some windows, and for the same reasons, we do not
;;; define a class for applications.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function UPDATE-WINDOW-FROM-VIEW.
;;;
;;; Client code will define a primary method on this generic function,
;;; specialized to the class of the window used by the interface
;;; manager for displaying the buffer contents according to the view.

(defgeneric update-window-from-view (window view))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function UPDATE-WINDOW.
;;;
;;; This function is called from the main application command loop or
;;; event loop in order to update a visible window.  A :BEFORE method
;;; makes sure that the analyzer and the view of the window are
;;; updated.  The default method on this function ignores the
;;; application object and calls UPDATE-WINDOW-FROM-VIEW with the
;;; window and the view of the window.

(defgeneric update-window (application window))

(defmethod update-window (application window)
  (declare (ignore application))
  (update-window-from-view window (view window)))

(defmethod update-window :before (application window)
  (update-analyzer (analyzer (view window)))
  (update-view (view window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VIEW.
;;;
;;; Given a window, return the Climacs view for that window.

(defgeneric view (window))
