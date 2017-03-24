(cl:in-package #:climacs-clim-view-fundamental)

(stealth-mixin:define-stealth-mixin
    output-history
    (clim:output-record clim:stream-output-history-mixin)
  climacs-syntax-fundamental:analyzer
  ((%parent :initarg :parent :accessor clim:output-record-parent)
   (%max-widths-prefix :initform '() :accessor max-widths-prefix)
   (%max-widths-suffix :initform '() :accessor max-widths-suffix)))

(defmethod climacs-syntax-fundamental:push-to-prefix :after
    ((analyzer output-history) entry)
  (with-accessors ((prefix climacs-syntax-fundamental:prefix)) analyzer
    (with-accessors ((contents climacs-syntax-fundamental:contents)
                     (list-length climacs-syntax-fundamental:list-length))
        entry
      (push (max (if (null (rest prefix))
                     0
                     (first (max-widths-prefix analyzer)))
                 (length contents))
            (max-widths-prefix analyzer)))))

(defmethod climacs-syntax-fundamental:pop-from-prefix :after
    ((analyzer output-history))
  (pop (max-widths-prefix analyzer)))

(defmethod climacs-syntax-fundamental:push-to-suffix :after
    ((analyzer output-history) entry)
  (with-accessors ((suffix climacs-syntax-fundamental:suffix)) analyzer
    (with-accessors ((contents climacs-syntax-fundamental:contents)
                     (list-length climacs-syntax-fundamental:list-length))
        entry
      (push (max (if (null (rest suffix))
                     0
                     (first (max-widths-suffix analyzer)))
                 (length contents))
            (max-widths-suffix analyzer)))))

(defmethod climacs-syntax-fundamental:pop-from-suffix :after
    ((analyzer output-history))
  (pop (max-widths-suffix analyzer)))

(defclass fundamental-view (second-climacs-clim-base:climacs-clim-view)
  ())

(defmethod second-climacs-clim-base:make-climacs-clim-view
    ((view climacs-syntax-fundamental:view))
  (let ((analyzer (climacs2-base:analyzer view)))
    (make-instance 'fundamental-view
      :output-history analyzer
      :climacs-view view)))

(defgeneric draw-line (pane analyzer contents line-number))

(defmethod draw-line (pane (analyzer output-history) contents line-number)
  (let* ((text-style (clim:medium-text-style pane))
         (text-height (clim:text-style-height text-style pane))
         (text-width (clim:text-style-width text-style pane))
         (text-ascent (clim:text-style-ascent text-style pane))
         (y (+ text-ascent (* line-number text-height)))
         (clim-view (clim:stream-default-view pane))
         (climacs-view (second-climacs-clim-base:climacs-view clim-view))
         (cursor (climacs2-base:cursor climacs-view))
         (cursor-column-number (cluffer:cursor-position cursor)))
    (if (= (cluffer:line-number cursor) line-number)
        (cond ((zerop cursor-column-number)
               (clim:draw-rectangle* pane 1 (- y text-height) 4 y
                                     :ink clim:+blue+)
               (unless (zerop (length contents))
                 (clim:draw-text* pane contents 5 y)))
              ((= cursor-column-number (length contents))
               (unless (zerop (length contents))
                 (clim:draw-text* pane contents 0 y))
               (let ((cursor-x (* (length contents) text-width)))
                 (clim:draw-rectangle* pane
                                       (1+ cursor-x) (- y text-height)
                                       (+ cursor-x 4) y
                                       :ink clim:+blue+)))
              (t
               (clim:draw-text* pane contents 0 y
                                :start 0
                                :end cursor-column-number)
               (let ((cursor-x (* cursor-column-number text-width)))
                 (clim:draw-rectangle* pane
                                       (1+ cursor-x) (- y text-height)
                                       (+ cursor-x 4) y
                                       :ink clim:+blue+)
                 (clim:draw-text* pane contents (+ cursor-x 5) y
                                  :start cursor-column-number))))
        (clim:draw-text* pane contents 0 y))))

(defun adjust-for-rendering (analyzer last-line)
  (with-accessors ((prefix climacs-syntax-fundamental:prefix)
                   (suffix climacs-syntax-fundamental:suffix))
      analyzer
    (loop until (or (null suffix)
                    (>= (climacs-syntax-fundamental:list-length prefix)
                        last-line))
          do (climacs-syntax-fundamental:suffix-to-prefix analyzer))
    (loop until (or (null prefix)
                    (<= (climacs-syntax-fundamental:list-length prefix)
                        last-line))
          do (climacs-syntax-fundamental:prefix-to-suffix analyzer))))

(defgeneric render (analyzer pane first-line last-line))

(defmethod render ((analyzer output-history) pane first-line last-line)
  (adjust-for-rendering analyzer last-line)
  (loop with prefix = (climacs-syntax-fundamental:prefix analyzer)
        for entry in prefix
        for line-number = (climacs-syntax-fundamental:list-length entry)
        for contents = (climacs-syntax-fundamental:contents entry)
        until (< line-number first-line)
        do (draw-line pane analyzer contents line-number)))

(defun line-count (analyzer)
  (with-accessors ((prefix climacs-syntax-fundamental:prefix)
                   (suffix climacs-syntax-fundamental:suffix))
      analyzer
    (+ (if (null prefix)
           0
           (climacs-syntax-fundamental:list-length (first prefix)))
       (if (null suffix)
           0
           (climacs-syntax-fundamental:list-length (first suffix))))))

(defmethod clim:replay-output-record
    ((analyzer output-history) stream &optional region x-offset y-offset)
  (declare (ignore x-offset y-offset region))
  (multiple-value-bind (left top right bottom)
      (clim:bounding-rectangle* (clim:pane-viewport-region stream))
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
  (with-accessors ((prefix climacs-syntax-fundamental:prefix)
                   (suffix climacs-syntax-fundamental:suffix))
    history
    (let* ((stream (clim:output-record-parent history))
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

(defmethod second-climacs-clim-base:update-view (pane (view fundamental-view))
  (let ((history (clim:stream-output-history pane)))
    (climacs2-base:update-view (second-climacs-clim-base:climacs-view view))
    (clim:with-bounding-rectangle* (x1 y1 x2 y2) history
      (declare (ignore x1 y1))
      (clim:change-space-requirements
       (clim:output-record-parent history)
       :width x2
       :height y2))
    (clim:replay history pane)))
