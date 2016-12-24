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

(defgeneric update-window-from-view (window view))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function UPDATE-WINDOW.

(defgeneric update-window (application window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VIEW.
;;;
;;; Given a window, return the Climacs view for that window.

(defgeneric view (window))

