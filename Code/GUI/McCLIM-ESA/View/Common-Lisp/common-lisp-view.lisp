(cl:in-package #:second-climacs-clim-view-common-lisp)

(stealth-mixin:define-stealth-mixin
    output-history
    (clim:output-record clim:stream-output-history-mixin)
  ip:cache
  ())

(defclass common-lisp-view (clim-base:climacs-clim-view)
  ())

(defmethod clim-base:make-climacs-clim-view ((view cl-syntax:view))
  (let* ((analyzer (base:analyzer view))
         (cache (ip:cache analyzer)))
    (make-instance 'common-lisp-view :output-history cache
                                     :climacs-view view)))

(defmethod clim-base:command-table ((view cl-syntax:view))
  (let ((buffer (ip:buffer (base:analyzer view))))
    (if (edit.search:search-state buffer)
        (clim:find-command-table 'clim-base::incremental-search-table)
        (clim:find-command-table 'common-lisp-table))))

(defgeneric render-cache (cache pane min-line min-column max-line max-column))

;;; This variable is used to accumulate ERROR-WAD instances as they
;;; are encountered while drawing visible wads.
(defvar *error-wads*)

(defmethod render-cache ((cache output-history) pane
                         min-line min-column max-line max-column)
  (let* ((view         (clim:stream-default-view pane))
         (climacs-view (clim-base:climacs-view view))
         (analyzer     (base:analyzer climacs-view))
         (buffer       (ip:buffer analyzer))
         (context      (make-instance
                        'context
                        :stream             pane
                        :min-line           min-line
                        :min-column         min-column
                        :min-column/content (first-non-whitespace-as-min-column
                                             buffer)
                        :max-line           max-line
                        :max-column         max-column
                        :max-column/content (line-length-as-max-colum buffer))))
    ;; Draw region rectangles first, that is beneath everything.
    (draw-regions context buffer)
    ;; Draw wads and buffer content.
    (let ((*error-wads* '()))
      ;; Draw wads, noting any errors.
      (ip:map-wads-and-spaces
       cache min-line max-line
       (lambda (wad)
         (draw-wad context wad (ip:absolute-start-line wad) cache))
       (lambda (line start-column end-column)
         (draw-interval
          context line (ip:line-contents cache line)
          (max start-column min-column) (min end-column max-column))))
      ;; Draw error decorations (in buffer) annotations (near cursor)
      ;; and gutter indicators.
      (draw-error-wads context *error-wads*))
    ;; Draw point and mark cursors and possibly search state atop
    ;; everything else.
    (draw-cursors context buffer)
    (draw-search-state context buffer)))

;;; TODO Doesn't work completely right. Disabled for now.
#+(or) (defmethod draw-wad :before (context wad start-ref cache)
         (declare (ignore cache))
         (let* ((indentation (ip:indentation wad))
                (start-column (ip:start-column wad))
                (pane (stream* context))
                (gutter (clim-base:left-gutter pane)))
           (cond ((null indentation)
                  nil)
                 ((= indentation start-column)
                  nil)
                 ((< indentation start-column)
                  (draw-left-arrow context gutter start-ref clim:+blue+))
                 (t
                  (draw-right-arrow context gutter start-ref clim:+blue+)))))

(defmethod draw-wad :before (context (wad ip:wad) start-ref cache)
  (declare (ignore start-ref cache))
  ;; Record encountered error wads so that the various error
  ;; indicators can be drawn later.
  ;; TODO `append'ing like this is slow
  ;; must copy since `nreverse'd later
  (alexandria:when-let ((errors (ip:errors wad)))
    (setf *error-wads* (append *error-wads* (copy-list errors)))))

;;; Return the area of the viewport of PANE in units of line and
;;; column number.  We return only integers, so that if a fraction of
;;; a line or a column is included in the viewport, then the entire
;;; line or column is included in the return values.  Four values are
;;; returned: The column and the line of the upper-left corner and the
;;; column and the line of the lower-right corner.
(defun viewport-area (pane)
  (let ((region (clim:pane-viewport-region pane)))
    (clim:with-bounding-rectangle* (left top right bottom) region
      (multiple-value-bind (width height) (text-style-dimensions pane)
        (values (floor left width) (floor top height)
                (ceiling right width) (ceiling bottom height))))))

(defun clear-viewport (pane)
  (let ((region (clim:pane-viewport-region pane)))
    (clim:with-bounding-rectangle* (left top right bottom) region
      (clim:medium-clear-area (clim:sheet-medium pane)
                              left top right bottom))))

(defmethod clim:replay-output-record
    ((cache output-history) stream &optional region x-offset y-offset)
  (declare (ignore x-offset y-offset region))
  (clear-viewport stream)
  (multiple-value-bind (left top right bottom) (viewport-area stream)
    (let* ((min-column  (max 0 left))
           (line-count  (ip:line-count cache))
           (max-line    (min bottom (1- line-count)))
           (total-width (ip:total-width cache))
           (max-column  (min right total-width)))
      (unless (minusp max-line)
        (render-cache cache stream top min-column max-line max-column)))))

(defmethod clim:bounding-rectangle* ((history output-history))
  (let ((pane (climi::output-history-stream history)))
    (multiple-value-bind (width height) (text-style-dimensions pane)
      (let* ((line-count (ip:line-count history))
             (total-width (ip:total-width history)))
        (values 0 0 (* width total-width) (* height line-count))))))

;;; I don't know why this one is called at all
(defmethod clim:clear-output-record ((history output-history))
  nil)

(defmethod clim-base:update-view (pane (view common-lisp-view))
  (progn ; clim-internals::with-output-buffered (pane)
    (let ((history (clim:stream-output-history pane))
          (gutter (clim-base:left-gutter pane)))
      (base:update-view (clim-base:climacs-view view))
      (let ((stream (climi::output-history-stream history)))
        (clim:with-bounding-rectangle* (:x2 x2 :y2 y2) history
          (clim:change-space-requirements stream :width x2 :height y2))
        (clim:clear-output-record (clim:stream-output-history gutter))
        (clim:window-erase-viewport gutter))
      (clim:replay history pane))))

(defmethod clim:output-record-parent ((record output-history))
  nil)

(defmethod clim:output-record-count ((record output-history))
  0)

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

(defmethod base:update-view-from-analyzer ((view cl-syntax:view)
                                           (analyzer ip:analyzer))
  (ip:update analyzer))
