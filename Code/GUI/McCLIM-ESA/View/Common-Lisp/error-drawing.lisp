(cl:in-package #:second-climacs-clim-view-common-lisp)

;;; The purpose of this class is to capture the absolute start line of
;;; a wad as the wad is being drawn.  For a relative wad, the absolute
;;; line is not readily available outside of the drawing process.
(defclass drawn-wad ()
  ((%wad        :initarg :wad
                :reader  wad)
   (%start-line :initarg :start-line
                :reader  start-line)))

(defmethod end-column ((drawn-wad drawn-wad))
  (let* ((wad       (wad drawn-wad))
         (condition (ip::condition* wad)))
    (+ (ip:start-column wad)
       (if (typep condition 'end-of-file)
           0
           1))))

(defmethod print-object ((object drawn-wad) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D" (start-line object))))

;;; This variable is used to accumulate ERROR-WAD instances as they
;;; are drawn.  Each such wad is wrapped in a DRAWN-WAD instance in
;;; order to capture the absolute start line.
(defvar *drawn-error-wads* '())

(defmethod render-cache :around ((cache output-history)
                                 pane
                                 first-line
                                 last-line)
  (let ((*drawn-error-wads* '()))
    (call-next-method)
    (let* ((error-wads     (nreverse *drawn-error-wads*))
           (line-clusters  (cluster-error-wads
                            (lambda (line end-column
                                     previous-line previous-end-column)
                              (declare (ignore end-column previous-end-column))
                              (= previous-line line))
                            error-wads))
           (exact-clusters (cluster-error-wads
                            (lambda (line end-column previous-line
                                     previous-end-column)
                              (and (= previous-line line)
                                   (= end-column previous-end-column)))
                            error-wads)))
      (draw-gutter-indicators pane line-clusters)
      (draw-error-wad-decorations pane exact-clusters)
      (format-error-annotations pane exact-clusters))))

(defmethod draw-wad :around ((wad ip:error-wad)
                             start-ref pane cache first-line last-line)
  (declare (ignore start-ref cache first-line last-line))
  ;; Record WAD and its absolute start line for processing.
  (push (make-instance 'drawn-wad :wad wad :start-line start-ref)
        *drawn-error-wads*)
  (call-next-method))

;;; Error clustering

(defun cluster-error-wads (predicate error-wads)
  (let ((clusters '())
        (cluster  '()))
    (flet ((add (wad)
             (push wad cluster))
           (flush ()
             (unless (null cluster)
               (push (nreverse cluster) clusters))
             (setf cluster '())))
      (loop :for previous-line       = nil :then line
            :for previous-end-column = nil :then end-column
            :for drawn-wad           :in error-wads
            :for line                = (start-line drawn-wad)
            :for end-column          = (end-column drawn-wad)
            :when (not (or (null cluster)
                           (null previous-line)
                           (funcall predicate
                                    line end-column
                                    previous-line previous-end-column)))
              :do (flush)
            :do (add drawn-wad)
            :finally (flush)))
    (nreverse clusters)))

;;; Gutter indicators

(defun draw-gutter-indicators (pane error-wad-clusters)
  ;; Draw error indicator in gutter.
  ;; TODO this is a temporary solution. It would be better to inform
  ;; the gutter pane about errors and let it handle the drawing
  ;; itself.
  (let ((gutter (second-climacs-clim-base::left-gutter pane)))
    (multiple-value-bind (width height ascent) (text-style-dimensions pane)
      (loop :for cluster    :in error-wad-clusters
            :for count      =   (length cluster)
            :for drawn-wad  =   (first cluster)
            :for start-line =   (start-line drawn-wad)
            :for y          =   (* start-line height)
            :for baseline   =   ascent
            :do (clim:with-translation (gutter 0 y)
                  (clim:draw-rectangle* gutter 0 0 (+ width 1) height
                                        :ink clim:+red+)
                  (when (> count 1)
                    (let ((label (princ-to-string count)))
                      (clim:draw-text* gutter label 1 baseline
                                       :text-size :smaller
                                       :align-y   :baseline))))))))

;;; Wad decorations

(defun draw-error-wad-decorations (stream error-wad-clusters)
  (mapc (alexandria:curry #'draw-error-wad-cluster-decoration stream)
        error-wad-clusters))

(defun draw-error-wad-cluster-decoration (stream error-wad-cluster)
  (multiple-value-bind (width height ascent) (text-style-dimensions stream)
    (let* ((start-line   (start-line (first error-wad-cluster)))
           (wads         (mapcar #'wad error-wad-cluster))
           (start-column (reduce #'min wads :key #'ip:start-column))
           (end-column   (reduce #'max error-wad-cluster :key #'end-column))
           (y            (+ ascent (* start-line height)))
           (x            (* start-column width)))
      (draw-error-decoration stream x y start-column end-column width))))

(defun draw-error-decoration
    (stream x y start-column end-column character-width)
  (let ((line-height  (nth-value 1 (text-style-dimensions stream)))
        (column-count (- end-column start-column)))
    (if (plusp column-count)
        (let ((thickness (* 1/8 line-height))
              (width     (* character-width column-count)))
          (clim:draw-line* stream x (+ y thickness) (+ x width) (+ y thickness)
                           :line-thickness thickness :ink clim:+red+))
        (let* ((size   (* 1/4 line-height))
               (points (vector x y
                               (+ x size) (+ y size)
                               (- x size) (+ y size))))
          (clim:draw-polygon* stream points :ink clim:+red+)))))

;;; Error annotations

(defun format-error-annotations (stream error-wad-clusters)
  ;; TODO get the cursor in some other way
  (let* ((clim-view    (clim:stream-default-view stream))
         (climacs-view (clim-base:climacs-view clim-view))
         (cursor       (base:cursor climacs-view)))
    (multiple-value-bind (cursor-line cursor-column)
        (base:cursor-positions cursor)
      (loop :for cluster    :in error-wad-clusters
            :for error-wad  =   (first cluster)
            :for wad        =   (wad error-wad)
            :for start-line =   (start-line error-wad)
            :when (and (= start-line cursor-line)
                       (<= (ip:start-column wad)
                           cursor-column
                           (end-column error-wad)))
              :do (format-error-cluster-annotation stream cluster)))))

(defun format-error-cluster-annotation (stream error-wad-cluster)
  (multiple-value-bind (width height ascent) (text-style-dimensions stream)
    (let* ((error-wad    (first error-wad-cluster))
           (wad          (wad error-wad))
           (start-line   (start-line error-wad))
           (start-column (ip:start-column wad))
           (y            (+ ascent (* start-line height) (* 1/2 height)))
           (x            (* start-column width))
           (conditions   (mapcar (alexandria:compose
                                  #'ip::condition* #'wad)
                                 error-wad-cluster)))
      (format-error-conditions-annotation stream x y conditions))))

;;; Use "tints" so that the background and border of the annotation
;;; work for different pane backgrounds.
(defvar *pink-tint* (clim:compose-in clim:+pink+ (clim:make-opacity .5)))
(defvar *salmon-tint* (clim:compose-in clim:+salmon+ (clim:make-opacity .5)))

(defun format-error-conditions-annotation (stream x y conditions)
  (clim:with-bounding-rectangle* (:width width :x2 x2)
      (clim:region-intersection
       (clim:pane-viewport-region stream) (clim:sheet-region stream))
    (let* ((padding 8)
           (width   (- (* 8/10 width) 6 (* 2 padding)))
           (x       (max padding (min x (- x2 width padding)))))
      (setf (clim:stream-cursor-position stream) (values x y))
      (let* ((pane-background (clim:pane-background stream))
             (background      (clim:compose-over
                               *pink-tint* pane-background))
             (border          (clim:compose-over
                               *salmon-tint* pane-background)))
        (clim:surrounding-output-with-border (stream :shape      :rounded
                                                     :radius     3
                                                     :background background
                                                     :ink        border)
          (format-error-list stream conditions :width width))))))

(defun format-error-list (stream conditions &key width)
  (clim:formatting-item-list (stream :n-columns 1)
    (loop :for condition :in conditions
          :do (clim:formatting-cell (stream)
                (when (> (length conditions) 1)
                  (write-string "• " stream))
                (let ((x (clim:stream-cursor-position stream)))
                  (clime:with-temporary-margins
                      (stream :left `(:absolute ,x) :right `(:absolute ,width))
                    (clim:with-end-of-line-action (stream :wrap*)
                      (princ condition stream))))))))
