(cl:in-package #:climacs-esa-gui)

(stealth-mixin:define-stealth-mixin
    output-history
    (clim:output-record clim:stream-output-history-mixin)
  climacs-syntax-common-lisp:cache
  ((%parent :initarg :parent :accessor clim:output-record-parent)
   ;; When this parse result is an element of the PREFIX, this slot
   ;; contains the the length of the longest line of all the lines
   ;; from the beginning of the buffer and up to and including the
   ;; last line of this parse result.  When this parse result is an
   ;; element of the SUFFIX, this slot contains the the length of the
   ;; longest line of all the lines from the first line of this parse
   ;; result to the end of the buffer.  We can determine the width of
   ;; the entire buffer by taking the MAX of the values of these lots
   ;; for the first element of the prefix, the first element of the
   ;; suffix, and all the lines in the buffer in between the last line
   ;; of the first element of the prefix and the first line of the
   ;; first element of the suffix.
   (%max-line-width-list :accessor max-line-width-list)))

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
  ()
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
