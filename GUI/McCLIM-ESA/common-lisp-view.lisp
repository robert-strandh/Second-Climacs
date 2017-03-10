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
         (text-ascent (clim:text-style-ascent text-style pane))
         (y (+ text-ascent (* line-number text-height)))
         (x (* start-column text-width)))
    (unless (= start-column end-column)
      (clim:draw-text* pane contents x y :start start-column :end end-column))))

(defun draw-area (pane
                  cache
                  start-line-number
                  start-column-number
                  end-line-number
                  end-column-number)
  (if (= start-line-number end-line-number)
      (let* ((contents (climacs-syntax-common-lisp:line-contents
                        cache start-line-number))
             (string (coerce contents 'string)))
        (draw-interval pane
                       start-line-number
                       string
                       start-column-number
                       end-column-number))
      (progn (let* ((first (climacs-syntax-common-lisp:line-contents
                            cache start-line-number))
                    (string (coerce first 'string)))
               (draw-interval pane
                              start-line-number
                              string
                              start-column-number
                              (length first)))
             (let* ((last (climacs-syntax-common-lisp:line-contents
                           cache end-line-number))
                    (string (coerce last 'string)))
               (draw-interval pane
                              end-line-number
                              string
                              0
                              end-column-number))
             (loop for line-number from (1+ start-line-number)
                     to (1- end-line-number)
                   for contents = (climacs-syntax-common-lisp:line-contents
                                   cache line-number)
                   for string = (coerce contents 'string)
                   do (draw-interval pane
                                     line-number
                                     string
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

(defmethod clim:replay-output-record
    ((cache output-history) stream &optional region x-offset y-offset)
  (declare (ignore x-offset y-offset region))
  (multiple-value-bind (left top right bottom)
      (clim:bounding-rectangle* (clim:pane-viewport-region stream))
    (clim:medium-clear-area (clim:sheet-medium stream)
                            left top right bottom)
    (let* ((text-style (clim:medium-text-style stream))
           (text-style-height (clim:text-style-height text-style stream))
           (first-line-number (floor top text-style-height))
           (line-count (climacs-syntax-common-lisp:line-count cache))
           (last-line-number (min (ceiling bottom text-style-height)
                                  (1- line-count))))
      (unless (minusp last-line-number)
        (let* ((last-line-contents (climacs-syntax-common-lisp:line-contents
                                    cache last-line-number))
               (last-line-length (length last-line-contents)))
          (draw-area stream
                     cache
                     first-line-number
                     0
                     last-line-number
                     last-line-length))))))

(defmethod clim:bounding-rectangle* ((history output-history))
  (let* ((stream (clim:output-record-parent history))
         (text-style (clim:medium-text-style stream))
         (text-style-height (clim:text-style-height text-style stream))
         ;; (text-style-width (clim:text-style-width text-style stream))
         (line-count (climacs-syntax-common-lisp:line-count history)))
    (values 0 0 500 (* text-style-height line-count))))

;;; I don't know why this one is called at all
(defmethod clim:clear-output-record ((history output-history))
  nil)

(defmethod update-view (pane (view common-lisp-view))
  (let ((history (clim:stream-output-history pane)))
    (climacs2-base:update-view (climacs-view view))
    (clim:with-bounding-rectangle* (x1 y1 x2 y2) history
      (declare (ignore x1 y1))
      (clim:change-space-requirements
       (clim:output-record-parent history)
       :width x2
       :height y2))
    (clim:replay history pane)))

(defmethod clim:map-over-output-records-containing-position
    (function
     (history output-history)
     x y
     &optional
       x-offset
       y-offset
     &rest function-args)
  (declare (ignore x-offset y-offset))
  ;; For now, do nothing.
  nil)

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
