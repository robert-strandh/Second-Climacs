(cl:in-package #:second-climacs-clim-view-fundamental)

(stealth-mixin:define-stealth-mixin
    output-history
    (clim:output-record clim:stream-output-history-mixin)
  fundamental-syntax:analyzer
  ((%parent :initarg :parent :accessor clim:output-record-parent)
   (%max-widths-prefix :initform '() :accessor max-widths-prefix)
   (%max-widths-suffix :initform '() :accessor max-widths-suffix)))

(defmethod clim:output-record-count ((record output-history))
  0)

(defmethod fundamental-syntax:push-to-prefix :after
    ((analyzer output-history) entry)
  (with-accessors ((prefix fundamental-syntax:prefix)) analyzer
    (with-accessors ((contents fundamental-syntax:contents)
                     (list-length fundamental-syntax:list-length))
        entry
      (push (max (if (null (rest prefix))
                     0
                     (first (max-widths-prefix analyzer)))
                 (length contents))
            (max-widths-prefix analyzer)))))

(defmethod fundamental-syntax:pop-from-prefix :after
    ((analyzer output-history))
  (pop (max-widths-prefix analyzer)))

(defmethod fundamental-syntax:push-to-suffix :after
    ((analyzer output-history) entry)
  (with-accessors ((suffix fundamental-syntax:suffix)) analyzer
    (with-accessors ((contents fundamental-syntax:contents)
                     (list-length fundamental-syntax:list-length))
        entry
      (push (max (if (null (rest suffix))
                     0
                     (first (max-widths-suffix analyzer)))
                 (length contents))
            (max-widths-suffix analyzer)))))

(defmethod fundamental-syntax:pop-from-suffix :after
    ((analyzer output-history))
  (pop (max-widths-suffix analyzer)))

(defclass fundamental-view (clim-base:climacs-clim-view)
  ())

(defmethod clim-base:make-climacs-clim-view
    ((view fundamental-syntax:view))
  (let ((analyzer (base:analyzer view)))
    (make-instance 'fundamental-view
      :output-history analyzer
      :climacs-view view)))

(defgeneric draw-line (pane analyzer contents line-number))

(defun draw-cursor (pane x y height)
  (clim:draw-rectangle* pane (1- x) (- y height) (+ x 2) y
                        :ink clim:+blue+))

(defmethod draw-line (pane (analyzer output-history) contents line-number)
  (let* ((text-style (clim:medium-text-style pane))
         (text-height (clim:text-style-height text-style pane))
         (text-width (clim:text-style-width text-style pane))
         (text-ascent (clim:text-style-ascent text-style pane))
         (y (+ text-ascent (* line-number text-height)))
         ; (clim-view (clim:stream-default-view pane))
         ; (climacs-view (clim-base:climacs-view clim-view))
         (buffer (base:buffer analyzer))
         (cursor (text.editing:point buffer))
         (cursor-column-number (cluffer:cursor-position cursor))
         (string (coerce contents 'string)))
    (if (= (cluffer:line-number cursor) line-number)
        (cond ((zerop cursor-column-number)
               (draw-cursor pane 1 y text-height)
               (unless (zerop (length string))
                 (clim:draw-text* pane string 0 y)))
              ((= cursor-column-number (length string))
               (unless (zerop (length string))
                 (clim:draw-text* pane string 0 y))
               (let ((cursor-x (* (length string) text-width)))
                 (draw-cursor pane cursor-x y text-height)))
              (t
               (unless (zerop (length string))
                 (clim:draw-text* pane string 0 y
                                  :start 0
                                  :end cursor-column-number))
               (let ((cursor-x (* cursor-column-number text-width)))
                 (draw-cursor pane cursor-x y text-height)
               (unless (zerop (length string))
                 (clim:draw-text* pane string cursor-x y
                                  :start cursor-column-number)))))
        (unless (zerop (length string))
          (clim:draw-text* pane string 0 y)))))

(defun adjust-for-rendering (analyzer last-line)
  (with-accessors ((prefix fundamental-syntax:prefix)
                   (suffix fundamental-syntax:suffix))
      analyzer
    (loop until (or (null suffix)
                    (>= (1- (fundamental-syntax:list-length prefix))
                        last-line))
          do (fundamental-syntax:suffix-to-prefix analyzer))
    (loop until (or (null prefix)
                    (<= (1- (fundamental-syntax:list-length prefix))
                        last-line))
          do (fundamental-syntax:prefix-to-suffix analyzer))))

(defgeneric render (analyzer pane first-line last-line))

(defmethod render ((analyzer output-history) pane first-line last-line)
  (adjust-for-rendering analyzer last-line)
  (loop with prefix = (fundamental-syntax:prefix analyzer)
        for entry in prefix
        for line-number = (1- (fundamental-syntax:list-length entry))
        for contents = (fundamental-syntax:contents entry)
        until (< line-number first-line)
        do (draw-line pane analyzer contents line-number)))

(defun line-count (analyzer)
  (with-accessors ((prefix fundamental-syntax:prefix)
                   (suffix fundamental-syntax:suffix))
      analyzer
    (+ (if (null prefix)
           0
           (fundamental-syntax:list-length (first prefix)))
       (if (null suffix)
           0
           (fundamental-syntax:list-length (first suffix))))))

(defmethod clim:replay-output-record
    ((analyzer output-history) stream &optional region x-offset y-offset)
  (declare (ignore x-offset y-offset region))
  (multiple-value-bind (left top right bottom)
      (clim:bounding-rectangle*
       (clim:pane-viewport-region (clim:sheet-parent stream)))
    (clim:medium-clear-area (clim:sheet-medium stream)
                            left top right bottom)
    (let* ((text-style (clim:medium-text-style stream))
           (text-style-height (clim:text-style-height text-style stream))
           (first-line-number (floor top text-style-height))
           (line-count (line-count analyzer))
           (last-line-number (min (ceiling bottom text-style-height)
                                  (1- line-count))))
      (unless (minusp last-line-number)
        (render analyzer stream first-line-number last-line-number)))))

(defmethod clim:bounding-rectangle* ((history output-history))
  (with-accessors ((prefix fundamental-syntax:prefix)
                   (suffix fundamental-syntax:suffix))
    history
    (let* ((stream (climi::output-history-stream history))
           (text-style (clim:medium-text-style stream))
           (text-style-height (clim:text-style-height text-style stream))
           (text-style-width (clim:text-style-width text-style stream))
           (width (max (if (null prefix)
                           0
                           (first (max-widths-prefix history)))
                       (if (null suffix)
                           0
                           (first (max-widths-suffix history))))))
      (values 0
              0
              (* text-style-width width)
              (* text-style-height (line-count history))))))

;;; I don't know why this one is called at all
(defmethod clim:clear-output-record ((history output-history))
  nil)

(defmethod clim-base:update-view (pane (view fundamental-view))
  (let ((history (clim:stream-output-history pane)))
    (base:update-view (clim-base:climacs-view view))
    (clim:with-bounding-rectangle* (x1 y1 x2 y2) history
      (declare (ignore x1 y1))
      (clim:change-space-requirements
       (climi::output-history-stream history)
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
  (declare (ignore x-offset y-offset function-args))
  ;; For now, do nothing.
  nil)

(defun update-analyzer (view analyzer)
  (declare (ignore view))
  (let ((buffer (base:buffer analyzer)))
    (fundamental-syntax:scavenge analyzer buffer)))

(defmethod base:update-view-from-analyzer
    ((view fundamental-syntax:view)
     (analyzer fundamental-syntax:analyzer))
  (update-analyzer view analyzer))

(defmethod clim-base:command-table
    ((view  fundamental-syntax:view))
  (clim:find-command-table 'fundamental-table))
