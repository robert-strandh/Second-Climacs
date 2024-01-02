(cl:in-package #:second-climacs-clim-view-common-lisp)

(stealth-mixin:define-stealth-mixin
    output-history
    (clim:output-record clim:stream-output-history-mixin)
  ip:cache
  ())

(defclass common-lisp-view (clim-base:climacs-clim-view)
  ())

(defmethod clim-base:make-climacs-clim-view
    ((view cl-syntax:view))
  (let* ((analyzer (base:analyzer view))
         (cache (ip:cache analyzer)))
    (make-instance 'common-lisp-view :output-history cache
                                     :climacs-view view)))

;;; Given a vector and a position, if the position NIL, meaning the
;;; end of the vector, then return the length of the vector.
;;; Otherwise, return the original position.
(defun canonicalize-column-number (contents position)
  (if (null position)
      (length contents)
      position))

;;; Return the text-style width, the text-style height, and the ascent
;;; of PANE as three values.
(defun text-style-dimensions (pane)
  (let ((text-style (clim:medium-text-style pane)))
    (values (clim:text-style-width text-style pane)
            (clim:text-style-height text-style pane)
            (clim:text-style-ascent text-style pane)
            (clim:text-style-descent text-style pane))))

;;; Given a line number, return the Y coordinate of the base line of
;;; the line with that number.
(defun base-line-position (text-style pane line-number)
  (let ((text-height (clim:text-style-height text-style pane))
        (text-ascent (clim:text-style-ascent text-style pane)))
    (+ text-ascent (* line-number text-height))))

;;; Given a column number, return the X coordinate of the left edge of
;;; the column with that number.
(defun horizontal-position (text-style pane column-number)
  (* column-number (clim:text-style-width text-style pane)))

;;; Given a line number, a start-column-number, and an end-column
;;; number, return the rectangle coordinates of the corresponding text
;;; as four values: The X coordinate of the upper-left corner, the Y
;;; coordinate of the upper-left corner, the X coordinate of the
;;; lower-right corner, and the Y coordinate of the lower-right
;;; corner.
(defun rectangle-coordinates
    (pane line-number start-column-number end-column-number)
  (multiple-value-bind (width height ascent) (text-style-dimensions pane)
    (values (* start-column-number width)
            (* line-number height)
            (* end-column-number width)
            (+ ascent (* line-number height)))))

;;; Draw a rectangle defined by the start column and end column of a
;;; single line of text.
(defun draw-rectangle
    (pane line-number start-column-number end-column-number ink)
  (multiple-value-bind (x1 y1 x2 y2)
      (rectangle-coordinates
       pane line-number start-column-number end-column-number)
    (clim:draw-rectangle* pane x1 y1 x2 y2 :ink ink)))

(defun arrow-y-coordinates (pane line-number)
  (multiple-value-bind (width height ascent) (text-style-dimensions pane)
    (declare (ignore width))
    (let* ((y1 (* line-number height))
           (y2 (+ ascent y1)))
      (values (round (* 0.5 (+ y1 y2)))
              (round (* 0.15 (- y2 y1)))
              (round (* 0.5 (- y2 y1)))))))

(defun draw-left-arrow (pane gutter line-number ink)
  (multiple-value-bind (middle h1 h2)
      (arrow-y-coordinates pane line-number)
    (clim:draw-polygon*
     gutter
     (list 12 (+ middle h1) 6 (+ middle h1) 6 (+ middle h2) 0 middle
           6 (- middle h2) 6 (- middle h1) 12 (- middle h1))
     :closed t :filled t :ink ink)))

(defun draw-right-arrow (pane gutter line-number ink)
  (multiple-value-bind (middle h1 h2)
      (arrow-y-coordinates pane line-number)
    (clim:draw-polygon*
     gutter
     (list 0 (+ middle h1) 6 (+ middle h1) 6 (+ middle h2) 12 middle
           6 (- middle h2) 6 (- middle h1) 0 (- middle h1))
     :closed t :filled t :ink ink)))

(defun draw-cursor (pane x y ascent descent)
  (let ((width (* 1/6 (+ ascent descent))))
    (clim:draw-rectangle* pane
                          (- x (* 1/3 width)) (- y ascent)
                          (+ x (* 2/3 width)) (+ y descent)
                          :ink clim:+blue+)))

;;; Draw an interval of text from a single line.  Optimize by not
;;; drawing anything if the defined interval is empty.  END-COLUMN can
;;; be NIL which means the end of CONTENTS.
(defun draw-interval (pane line-number contents start-column end-column)
  (multiple-value-bind (width height ascent descent) (text-style-dimensions pane)
    (let* ((x (* start-column width))
           (y (+ ascent (* line-number height)))
           (clim-view (clim:stream-default-view pane))
           (climacs-view (clim-base:climacs-view clim-view))
           (cursor (base:cursor climacs-view))
           (cursor-line-number (cluffer:line-number cursor))
           (cursor-column-number (cluffer:cursor-position cursor))
           (canonicalized-end-column-number
             (canonicalize-column-number contents end-column)))
      ;; Either draw the whole interval or split the interval so that
      ;; the cursor can be drawn between the two parts.
      (unless (= start-column canonicalized-end-column-number)
        (clim:draw-text* pane contents x y :start start-column
                                           :end end-column))
      ;; Maybe draw cursor rectangle. TODO just do this outside of wad drawing
      (when (and (= cursor-line-number line-number)
                 (<= start-column cursor-column-number end-column))
        (let ((cursor-x (* cursor-column-number width)))
          (draw-cursor pane cursor-x y ascent descent))))))

;;; Draw an area of text defined by START-LINE-NUMBER,
;;; START-COLUMN-NUMBER, END-LINE-NUMBER, and END-COLUMN-NUMBER.  The
;;; text is drawn on PANE, using the contents from CACHE.
;;; END-COLUMN-NUMBER is permitted to be NIL, meaning the end of the
;;; line designated by END-LINE-NUMBER.
(defun draw-area (pane
                  cache
                  start-line-number
                  start-column-number
                  end-line-number
                  end-column-number)
  (cond ((= start-line-number end-line-number)
         (let ((contents (ip:line-contents
                          cache start-line-number)))
           (declare (type string contents))
           (draw-interval pane
                          start-line-number
                          contents
                          start-column-number
                          end-column-number)))
        (t
         (let ((first (ip:line-contents
                       cache start-line-number)))
           (declare (type string first))
           (draw-interval pane
                          start-line-number
                          first
                          start-column-number
                          (length first)))
         (let ((last (ip:line-contents
                      cache end-line-number)))
           (declare (type string last))
           (draw-interval pane
                          end-line-number
                          last
                          0
                          end-column-number))
         (loop for line-number from (1+ start-line-number)
               to (1- end-line-number)
               for contents of-type string
                  = (ip:line-contents
                     cache line-number)
               do (draw-interval pane
                                 line-number
                                 contents
                                 0
                                 (length contents))))))

;;; The parameters START-LINE, START-COLUMN, END-LINE, and END-COLUMN
;;; together define an initial area.  END-COLUMN may be NIL, meaning
;;; the end of the line indicated by END-LINE.  FIRST-LINE and
;;; LAST-LINE together define the filter.  Four values are returned:
;;; the filtered start line, the filtered start column, the filtered
;;; end line, and the filtered end column.  The last return value may
;;; be NIL, indicating the end of the line indicated by the third
;;; return value.
(defun filter-area
    (start-line start-column end-line end-column first-line last-line)
  (let ((sl start-line)
        (sc start-column)
        (el end-line)
        (ec end-column))
    (when (> first-line start-line)
      (setf sl first-line
            sc 0))
    (when (< last-line end-line)
      (setf el last-line
            ec nil))
    (values sl sc el ec)))

(defun draw-filtered-area (pane cache
                           start-line-number start-column-number
                           end-line-number end-column-number
                           first-line last-line)
  (multiple-value-bind (sl sc el ec)
      (filter-area start-line-number start-column-number
                   end-line-number end-column-number
                   first-line last-line)
    (draw-area pane cache sl sc el ec)))

(defmethod clim-base:command-table
    ((view  cl-syntax:view))
  (clim:find-command-table 'common-lisp-table))

(stealth-mixin:define-stealth-mixin
    presentation
    (clim:standard-presentation)
  ip:wad
  ()
  (:default-initargs :single-box t :type t))

(defmethod draw-wad :around (wad start-ref pane cache first-line last-line)
  (declare (ignore cache first-line last-line))
  (let ((indentation (ip::indentation wad))
        (start-column (ip:start-column wad))
        (gutter (clim-base:left-gutter pane)))
    (cond ((null indentation)
           nil)
          ((= indentation start-column)
           nil)
          ((< indentation start-column)
           (draw-left-arrow pane gutter start-ref clim:+blue+))
          (t
           (draw-right-arrow pane gutter start-ref clim:+blue+))))
  (call-next-method))

(defgeneric render-cache (cache pane first-line last-line))

(defmethod render-cache ((cache output-history) pane first-line last-line)
  (ip:map-wads-and-spaces
   cache first-line last-line
   (lambda (wad)
     (draw-wad wad (ip:absolute-start-line-number wad)
               pane cache first-line last-line))
   (lambda (line start-column end-column)
     (draw-interval pane line
                    (ip:line-contents cache line)
                    start-column end-column))))

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
    (declare (ignore left right))
    (let* ((line-count (ip:line-count cache))
           (last-line-number (min bottom (1- line-count))))
      (unless (minusp last-line-number)
        (render-cache cache stream top last-line-number)))))

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
  (clim-internals::with-output-buffered (pane)
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

(defmethod base:update-view-from-analyzer
    ((view cl-syntax:view)
     (analyzer ip:analyzer))
  (ip:update-cache analyzer))
