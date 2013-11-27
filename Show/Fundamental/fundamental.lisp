(defpackage #:climacs-show-fundamental
  (:use #:common-lisp)
  (:import-from #:climacs-analyzer-fundamental
		#:fundamental-analyzer
		#:paragraphs
		#:lines
		#:buffer-line)
  (:export
   #:fundamental-show
   ))

(in-package #:climacs-show-fundamental)

;;;; This initial version is not incremental at all, and should be
;;;; considered a tempoary version just to test all the concepts.  A
;;;; huge improvement in performance could be obtained by keeping
;;;; unmodified paragraphs and completely recreating modified ones.

(defclass line ()
  ((%analyzer-line :initarg :analyzer-line :reader analyzer-line)
   (%zone :initarg :zone :reader zone)))

(defclass paragraph ()
  ((%analyzer-paragraph :initarg :analyzer-paragraph :reader analyzer-paragraph)
   (%zone :initarg :zone :reader zone)))

(defclass fundamental-show ()
  ((%text-style
    :initform (clim3:text-style :free :fixed :roman 12)
    :initarg :text-style
    :accessor text-style)
   (%text-color
    :initform (clim3:make-color 0.0 0.0 0.0)
    :initarg :text-color
    :accessor text-color)
   ;; This wrap zone is supplied by the creator of this show.
   (%wrap :initarg :wrap :reader wrap)
   ;; The analyzer is supplied by the creator of this show.
   (%analyzer :initarg :analyzer :reader analyzer)
   ;; The cursor is supplied by the creator of this show.
   (%cursor :initarg :cursor :reader cursor)
   (%paragraphs :initform '() :accessor paragraphs)
   (%analyzer-time :initform -1 :accessor analyzer-time)))
  
(defun zones-from-line (show line)
  (let* ((buffer-line (climacs-analyzer-fundamental:buffer-line line))
	 (items (climacs-buffer:items buffer-line))
	 (length (length items)))
    (if (eq (climacs-buffer:line (cursor show)) buffer-line)
	(let ((pos (climacs-buffer:cursor-position (cursor show)))
	      (cursor (clim3:brick
		       5 20
		       (clim3:opaque (clim3:make-color 1.0 0.0 0.0)))))
	  (cond ((= pos 0)
		 (clim3:hbox*
		  cursor
		  (clim3-text:text items
				   (text-style show)
				   (text-color show))
		  (clim3:sponge)))
		((= pos length)
		 (clim3:hbox*
		  (clim3-text:text items
				   (text-style show)
				   (text-color show))
		  cursor
		  (clim3:sponge)))
		(t
		 (clim3:hbox*
		  (clim3-text:text (subseq items 0 pos)
				   (text-style show)
				   (text-color show))
		  cursor
		  (clim3-text:text (subseq items pos)
				   (text-style show)
				   (text-color show))
		  (clim3:sponge)))))
	(clim3:hbox*
	 (clim3-text:text items
			  (text-style show)
			  (text-color show))
	 (clim3:sponge)))))
		    
(defun zones-from-paragraph (show paragraph)
  (clim3:vbox
   (loop for line in (climacs-analyzer-fundamental:lines paragraph)
	 collect (zones-from-line show line))))

(defun zones-from-analyzer (show analyzer)
  (clim3:vbox
   (loop for paragraph in (climacs-analyzer-fundamental:paragraphs analyzer)
	 collect (zones-from-paragraph show paragraph))))
	
(defmethod climacs-show:update ((show fundamental-show))
  (setf (clim3:children (wrap show))
	(zones-from-analyzer show (analyzer show))))

(defmethod climacs-show:make-show
    ((analyzer climacs-analyzer-fundamental:fundamental-analyzer)
     cursor
     wrap)
  (make-instance 'fundamental-show
    :analyzer analyzer
    :cursor cursor
    :wrap wrap))
