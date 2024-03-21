(cl:in-package #:second-climacs-clim-view-common-lisp)

;;; Position computations

(defun effective-column (designator line)
  (etypecase designator
    (real                 designator)
    ((or function symbol) (funcall designator line))))

(defun line-length-as-max-colum (buffer)
  (lambda (line-number)
    (let ((line (cluffer:find-line buffer line-number)))
      (cluffer:item-count line))))

(defun first-non-whitespace-as-min-column (buffer)
  (lambda (line-number)
    (let ((line (cluffer:find-line buffer line-number)))
      (or (position-if-not #'edit:whitespacep (cluffer:items line))
          0))))

;;; The parameters START-LINE, START-COLUMN, END-LINE, and END-COLUMN
;;; together define an initial area.  MIN-LINE, MIN-COLUMN, MAX-LINE,
;;; and MAX-COLUMN together define the filter.  MIN-COLUMN and
;;; MAX-COLUMN are designators accepted by `effective-column'.
;;; MAX-COLUMN may be `nil' to indicate that the length of line should
;;; be used to determine the maximum column.  Four values are
;;; returned: the filtered start line, the filtered start column, the
;;; filtered end line, and the filtered end column.
(defun filter-area (start-line start-column end-line end-column
                    min-line   min-column   max-line max-column)
  (multiple-value-bind (start-line start-column)
      (if (< start-line min-line)
          (values min-line   (effective-column min-column min-line))
          (values start-line (max start-column
                                  (effective-column min-column start-line))))
    (multiple-value-bind (end-line end-column)
        (if (< max-line end-line)
            (values max-line (if (null max-column)
                                 nil
                                 (effective-column max-column max-line)))
            (values end-line (if (null max-column)
                                 end-column
                                 (min end-column
                                      (effective-column
                                       max-column end-line)))))
      (values start-line start-column end-line end-column))))

(defun map-lines-in-range (function
                           start-line start-column end-line end-column
                           min-line   min-column   max-line max-column)
  (multiple-value-bind (start-line start-column end-line end-column)
      (filter-area start-line start-column end-line end-column
                   min-line   min-column   max-line max-column)
    (loop :for line :from start-line :to end-line
          :for start-column* = (if (= line start-line)
                                   start-column
                                   (effective-column min-column line))
          :for end-column* = (if (= line end-line)
                                 end-column
                                 (effective-column max-column line))
          :do (funcall function line start-column* end-column*))))

;;; Return the text-style width and the text-style height as two
;;; values.
(defun text-style-dimensions (pane)
  (let ((text-style (clim:medium-text-style pane)))
    (values (clim:text-style-width text-style pane)
            (clim:text-style-height text-style pane))))

;;; The drawing content

(defclass context ()
  ((%stream             :initarg  :stream
                        :reader   stream*)
   ;;
   (%character-width    :initarg  :character-width
                        :reader   character-width)
   (%line-height        :initarg  :line-height
                        :reader   line-height)
   (%ascent             :initarg  :ascent
                        :reader   ascent)
   (%descent            :initarg  :descent
                        :reader   descent)
   ;; Bounding rectangle of the buffer area being
   ;; drawn. `min-column/content' and `max-column/content' return
   ;; functions that can be passed to `effective-column' to compute
   ;; the min/max column of the (non-whitespace) content for a given
   ;; line.
   (%min-line           :initarg :min-line
                        :reader  min-line)
   (%min-column         :initarg :min-column
                        :reader  min-column)
   (%min-column/content :initarg :min-column/content
                        :reader  min-column/content)
   (%max-line           :initarg :max-line
                        :reader  max-line)
   (%max-column         :initarg :max-column
                        :reader  max-column)
   (%max-column/content :initarg :max-column/content
                        :reader  max-column/content))
  (:default-initargs
   :stream (error "required")))

(defmethod shared-initialize :after ((instance context) (slot-names t)
                                     &key (stream nil stream-supplied-p))
  (when stream-supplied-p
    (let ((text-style (clim:medium-text-style stream)))
      (setf (slot-value instance '%character-width)
            (clim:stream-character-width stream #\M)
            (slot-value instance '%line-height)
            (clim:text-style-height text-style stream)
            (slot-value instance '%ascent)
            (clim:text-style-ascent text-style stream)
            (slot-value instance '%descent)
            (clim:text-style-descent text-style stream)))))

(defun text-position (context line-number column-number &key include-ascent)
  (let ((line-height     (line-height context))
        (character-width (character-width context))
        (dy              (if include-ascent
                             (ascent context)
                             0)))
    (values (* character-width column-number)
            (+ (* line-height line-number) dy))))

;;; Convention for the following drawing functions: functions with
;;; "plain" name take line and column numbers, functions with * names
;;; take x and y coordinates.

;;; Underline

(defun draw-underline-marker
    (context start-line start-column end-line end-column
     &rest args
     &key line-thickness
          (min-column        (min-column/content context))
          (max-column        (max-column/content context))
          (empty-line-drawer 'draw-triangle-marker)
     &allow-other-keys)
  (let ((stream          (stream* context))
        (thickness       (or line-thickness (* 1/8 (line-height context))))
        (character-width (character-width context))
        (drawing-options (alexandria:remove-from-plist
                          args :line-thickness :min-column :max-column
                               :empty-line-drawer)))
    (map-lines-in-range
     (lambda (line start-column end-column)
       (cond ((= start-column end-column)
              (apply empty-line-drawer context line start-column drawing-options))
             ((< start-column end-column)
              (let* ((column-count (- end-column start-column))
                     (width        (* character-width column-count)))
                (multiple-value-bind (x y)
                    (text-position context line start-column :include-ascent t)
                  (apply #'clim:draw-line* stream x (+ y thickness) (+ x width) (+ y thickness)
                         :line-thickness thickness drawing-options))))))
     start-line start-column end-line end-column
     (min-line context) min-column (max-line context) max-column)))

;;; Triangle

(defun draw-triangle-marker* (stream x y
                              &rest drawing-options
                              &key (line-height (clim:text-style-height
                                                 (clim:medium-text-style stream)
                                                 stream))
                                   (height      1/4)
                                   filled
                              &allow-other-keys)
  (let* ((thickness 1)
         (size      (- (* height line-height) (if filled 0 (* 1/2 thickness))))
         (points    (vector x y
                            (+ x size) (+ y size)
                            (- x size) (+ y size))))
    (apply #'clim:draw-polygon* stream points
           (alexandria:remove-from-plist drawing-options :line-height :height))))

(defun draw-triangle-marker (context line column
                             &rest drawing-options &key &allow-other-keys)
  (multiple-value-bind (x y) (text-position context line column
                                            :include-ascent t)
    (let ((line-height (line-height context)))
      (apply #'draw-triangle-marker* (stream* context) x y
             :line-height line-height drawing-options))))

;;; Rectangle

;;; Given a line number, a start-column-number, and an end-column
;;; number, return the rectangle coordinates of the corresponding text
;;; as four values: The X coordinate of the upper-left corner, the Y
;;; coordinate of the upper-left corner, the X coordinate of the
;;; lower-right corner, and the Y coordinate of the lower-right
;;; corner.
(defun rectangle-coordinates (context line start-column end-column
                              &key include-descent)
  (let ((width  (* (character-width context) (- end-column start-column)))
        (height (if include-descent
                    (line-height context)
                    (ascent context))))
    (multiple-value-bind (x y) (text-position context line start-column)
      (values x y (+ x width) (+ y height)))))

;;; Draw a rectangle defined by the start column and end column of a
;;; single line of text.
(defun draw-one-line-rectangle (context line start-column end-column
                                &rest drawing-options
                                &key ink filled line-thickness
                                     (x2-offset 0) include-descent)
  (declare (ignore ink filled line-thickness))
  (multiple-value-bind (x1 y1 x2 y2)
      (rectangle-coordinates context line start-column end-column
                             :include-descent include-descent)
    (apply #'clim:draw-rectangle*
           (stream* context) x1 y1 (+ x2 x2-offset) y2
           (alexandria:remove-from-plist
            drawing-options :x2-offset :include-descent))))

(defun draw-multiple-line-rectangle
    (context start-line start-column end-line end-column
     &rest args &key (min-column (min-column context))
                     (max-column (max-column context))
                     ink filled line-thickness x2-offset include-descent)
  (declare (ignore ink filled line-thickness x2-offset include-descent))
  (let ((drawing-options (alexandria:remove-from-plist
                          args :min-column :max-column)))
    (map-lines-in-range
     (lambda (line start-column end-column)
       (when (< start-column end-column)
         (apply #'draw-one-line-rectangle context line start-column end-column
                drawing-options)))
     start-line start-column end-line end-column
     (min-line context) min-column (max-line context) max-column)))

;;; Arrows

(flet ((y-coordinates (context line)
         (let* ((y1 (* (line-height context) line))
                (y2 (+ y1 (ascent context))))
           (values (round (* 0.5 (+ y1 y2)))
                   (round (* 0.15 (- y2 y1)))
                   (round (* 0.5 (- y2 y1)))))))

  (defun draw-left-arrow (context gutter line-number ink)
    (multiple-value-bind (middle h1 h2) (y-coordinates context line-number)
      (clim:draw-polygon*
       gutter
       (list 12 (+ middle h1) 6 (+ middle h1) 6 (+ middle h2) 0 middle
             6 (- middle h2) 6 (- middle h1) 12 (- middle h1))
       :closed t :filled t :ink ink)))

  (defun draw-right-arrow (context gutter line-number ink)
    (multiple-value-bind (middle h1 h2) (y-coordinates context line-number)
      (clim:draw-polygon*
       gutter
       (list 0 (+ middle h1) 6 (+ middle h1) 6 (+ middle h2) 12 middle
             6 (- middle h2) 6 (- middle h1) 0 (- middle h1))
       :closed t :filled t :ink ink))))

;;; Annotation

(defun draw-annotation* (stream x y annotation-producer
                         &key (background clim:+white+)
                              (border     clim:+black+))
  (clim:with-bounding-rectangle* (:width width :x2 x2)
      (clim:region-intersection
       (clim:pane-viewport-region stream) (clim:sheet-region stream))
    (let* ((padding 8)
           (width   (- (* 8/10 width) 6 (* 2 padding)))
           (x       (max padding (min x (- x2 width padding)))))
      (setf (clim:stream-cursor-position stream) (values x y))
      (let* ((pane-background (clim:pane-background stream))
             (background      (clim:compose-over background pane-background))
             (border          (clim:compose-over border pane-background)))
        (clim:surrounding-output-with-border (stream :shape      :rounded
                                                     :radius     3
                                                     :background background
                                                     :ink        border)
          (funcall annotation-producer stream width))))))

(defun draw-annotation (context line column annotation-producer
                        &rest args &key background border)
  (declare (ignore background border))
  (multiple-value-bind (x y) (text-position context line column
                                            :include-ascent t)
    (let ((y (+ y (* 1/2 (line-height context)))))
      (apply #'draw-annotation* (stream* context) x y annotation-producer
             args))))
