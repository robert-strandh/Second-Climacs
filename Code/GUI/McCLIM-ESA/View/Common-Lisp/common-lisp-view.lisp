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
    (if (text.editing.search:search-state buffer)
        (clim:find-command-table 'clim-base::incremental-search-table)
        (clim:find-command-table 'common-lisp-table))))

(defgeneric render-cache (cache pane first-line last-line max-column))

;;; This variable is used to accumulate ERROR-WAD instances as they
;;; are encountered while drawing visible wads.
(defvar *error-wads*)

(defmethod render-cache ((cache output-history) pane
                         first-line last-line max-column)
  (let* ((view         (clim:stream-default-view pane))
         (climacs-view (clim-base:climacs-view view))
         (analyzer     (base:analyzer climacs-view))
         (buffer       (ip:buffer analyzer))
         (context      (make-instance 'context :stream pane)))
    ;; Draw region rectangles first, that is beneath everything.
    (draw-regions context buffer first-line last-line max-column)
    ;; Draw wads and buffer content.
    (let ((*error-wads* '()))
      ;; Draw wads, noting any errors.
      (ip:map-wads-and-spaces
       cache first-line last-line
       (lambda (wad)
         (draw-wad context wad (ip:absolute-start-line wad)
                   cache first-line last-line))
       (lambda (line start-column end-column)
         (draw-interval
          context line (ip:line-contents cache line) start-column end-column)))
      ;; Draw error decorations (in buffer) annotations (near cursor)
      ;; and gutter indicators.
      (draw-error-wads context *error-wads*))
    ;; Draw point and mark cursors and possibly search state atop
    ;; everything else.
    (draw-cursors context buffer first-line last-line)
    (draw-search-state context buffer first-line last-line)))

(defmethod draw-wad :before (context wad start-ref cache first-line last-line)
  (declare (ignore cache first-line last-line))
  (let* ((indentation (ip::indentation wad))
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

(defmethod draw-wad :before (context (wad ip:wad)
                             start-ref cache first-line last-line)
  (declare (ignore start-ref cache first-line last-line))
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
    (declare (ignore left))
    (let* ((line-count (ip:line-count cache))
           (last-line-number (min bottom (1- line-count))))
      (unless (minusp last-line-number)
        (render-cache cache stream top last-line-number right)))))

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
