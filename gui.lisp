(cl:in-package #:common-lisp-user)

(defpackage #:climacs-gui
  (:use #:common-lisp)
  (:export))

(in-package #:climacs-gui)

(defun status-line ()
  (let* ((background-color (clim3:make-color 0.7 0.7 0.7))
	 (background (clim3:opaque background-color))
	 (style (clim3:text-style :free :fixed :bold 12))
	 (color (clim3:make-color 0.0 0.0 0.0))
	 ;; Put in a text for testing purposes.
	 (text (clim3-text:text "-:**- climacs.lisp      0% (0,0) (Common Lisp)"
			   style color))
	 (thing (clim3:bboard* text))
	 (height (clim3:vframe 20 20 20)))
    (setf (clim3:depth background) 10)
    (clim3:pile* thing background height)))

(defun minibuffer ()
  (let* ((background-color (clim3:make-color 1.0 1.0 1.0))
	 (background (clim3:opaque background-color))
	 (thing (clim3:wrap))
	 (height (clim3:vframe 20 20 20)))
    (clim3:pile* thing background height)))

(defun text ()
  (let ((style (clim3:text-style :free :fixed :roman 12))
	(color (clim3:make-color 0.0 0.0 0.0)))
    ;; Put in a text for testing purposes.
    (let ((test-text (clim3-text:text "this is a test"
				      style
				      color)))
      (clim3:bboard* test-text))))
  
(defun climacs ()
  (let* ((size (clim3:brick 800 1000))
	 (tooltip (clim3:bboard*))
	 (text (text))
	 (status (status-line))
	 (minibuffer (minibuffer))
	 (info (clim3:vbox* text status minibuffer))
	 (all (clim3:pile* tooltip info size))
	 (port (clim3:make-port :clx-framebuffer))
	 (command-processor (make-instance 'emacs-style-command-processor
			      :mappings *mappings*))
	 (clim3:*key-handler*
	   (make-instance 'clim3::read-keystroke-key-handler
	     :receiver
	     (lambda (keystroke)
	       ;; Why can keystroke be NIL?
	       (unless (null keystroke)
		 (multiple-value-bind (answer additional)
		     (submit-keystroke command-processor keystroke)
		   (when (eq answer :complete-match)
		     (format t "~s~%" (cadr additional)))))))))
    (clim3:connect all port)
    (clim3:event-loop port)))

