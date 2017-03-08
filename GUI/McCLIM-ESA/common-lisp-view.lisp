(cl:in-package #:climacs-esa-gui)

(stealth-mixin:define-stealth-mixin
    output-history
    (clim:output-record clim:stream-output-history-mixin)
  climacs-syntax-common-lisp:cache
  ((%parent :initarg :parent :accessor clim:output-record-parent)))

(defclass common-lisp-view (climacs-clim-view)
  ((%previous-cursor-line-number
    :initform -1
    :accessor previous-cursor-line-number)
   (%previous-cursor-column-number
    :initform 0
    :accessor previous-cursor-column-number)))

(defmethod make-climacs-clim-view ((view climacs-syntax-common-lisp:view))
  (let* ((analyzer (climacs2-base:analyzer view))
         (cache (climacs-syntax-common-lisp:folio analyzer)))
    (make-instance 'common-lisp-view
      :output-history cache
      :climacs-view view)))

(defun draw-interval (pane line-number contents start-column end-column)
  (let* ((text-style (clim:medium-text-style pane))
	 (text-height (clim:text-style-height text-style pane))
	 (text-width (clim:text-style-width text-style pane))
	 (y (* line-number text-height))
	 (x (* start-column text-width)))
    (clim:draw-text* pane contents x y :start start-column :end end-column)))

(defun draw-area (pane
		  lines
		  start-line-number
		  start-column-number
		  end-line-number
		  end-column-number)
  (if (= start-line-number end-line-number)
      (let ((contents (cdr (flexichain:element* lines start-line-number))))
	(draw-interval pane
		       start-line-number
		       contents
		       start-column-number
		       end-column-number))
      (progn (let ((first (cdr (flexichain:element* lines start-line-number))))
	       (draw-interval pane
			      start-line-number
			      first
			      start-column-number
			      (length first)))
	     (let ((last (cdr (flexichain:element* lines end-line-number))))
	       (draw-interval pane
			      end-line-number
			      last
			      0
			      end-column-number))
	     (loop for line-number from (1+ start-line-number)
		     to (1- end-line-number)
		   for contents = (cdr (flexichain:element* lines line-number))
		   do (draw-interval pane
				     line-number
				     contents
				     0
				     (length contents))))))

(defmethod command-table
    ((view  climacs-syntax-common-lisp:view))
  (clim:find-command-table 'common-lisp-table))

(stealth-mixin:define-stealth-mixin
    presentation
    (clim:standard-presentation)
  climacs-syntax-common-lisp:parse-result
  (;; This slot contains the x-coordinate of the left border of the
   ;; presentation.  There might be buffer objects to the left of this
   ;; border, for instance if characters that are read are not part of
   ;; the output from the reader.  It typically won't happen, but it
   ;; might.  Either way, we only use the value of this slot for
   ;; highlighting, not for replaying.
   (%x-min :initform 0 :accessor x-min)
   ;; Similarly, this slot contains the x-coordinate of the right
   ;; border of the presentation.  There might be buffer objects to
   ;; the right of this border, but the value of this slot is only
   ;; used for highlighting.
   (%x-max :initform 0 :accessor x-max)
   ;; This slot contains the x-coordinate of the right edge of the
   ;; rightmost buffer object between the start and the end of the
   ;; parse result.  The value of this slot is used to determine the
   ;; total width of the pane that displays the buffer contents.
   (%width :initform 0 :accessor width)
   ;; This slot contains the height of the parse result.  The value of
   ;; this slot is used to determine the total height of the pane that
   ;; displays the buffer contents.
   (%height :initform 0 :accessor height)
   ;; This slot contains the maximum of all the values of all the
   ;; WIDTH slots in the parse results on either the prefix or the
   ;; suffix.  It is recomputed whenever a parse result is pushed to
   ;; one of those lists.
   (%max-width :initform 0 :accessor max-width)
   ;; This slot contains the total height of all the parse results on
   ;; either the prefix or the suffix.
   (%total-height :initform 0 :accessor total-height)
   ;; This slot contains the y-coordinate of the beginning of this
   ;; parse result.  It is computed when the parse result is pushed
   ;; onto the prefix, and it is only valid when the parse result is
   ;; an element of the prefix.  Thus, the output records that are
   ;; replayed, must all be on the prefix.
   (%top :initform 0 :accessor top))
  (:default-initargs :single-box t))

(defmethod climacs-syntax-common-lisp:push-to-prefix :before
    (cache (parse-result presentation))
  (with-accessors ((width1 width)
                   (max-width1 max-width)
                   (height1 height)
                   (total-height1 total-height)
                   (top1 top))
      parse-result
    (with-accessors ((prefix climacs-syntax-common-lisp:prefix)) cache
      (if (null prefix)
          (setf max-width1 width1
                total-height1 height1
                top1 0)
          (with-accessors ((width2 width)
                           (max-width2 max-width)
                           (height2 height)
                           (total-height2 total-height)
                           (top2 top))
              (first prefix)
            (setf max-width1 (max width1 max-width2)
                  ;; FIXME: This calculation is incorrect when
                  ;; the new parse result starts and the same line
                  ;; as the previous one ends.
                  top1 (+ top2 height2)
                  total-height1 (+ top1 height1)))))))

(defun update-cache (view pane analyzer)
  (declare (ignore view pane))
  (let* ((cache (climacs-syntax-common-lisp:folio analyzer))
	 (climacs-buffer (climacs2-base:buffer analyzer))
	 (cluffer-buffer (climacs2-base:cluffer-buffer climacs-buffer)))
    (climacs-syntax-common-lisp:scavenge cache cluffer-buffer)
    (climacs-syntax-common-lisp:read-forms analyzer)))

(defmethod climacs2-base:update-view-from-analyzer
    ((view climacs-syntax-common-lisp:view)
     (pane text-pane)
     (analyzer climacs-syntax-common-lisp:analyzer))
  (update-cache view pane analyzer))
