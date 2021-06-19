(cl:in-package #:second-climacs-clim-view-common-lisp)

(stealth-mixin:define-stealth-mixin
    output-history
    (clim:output-record clim:stream-output-history-mixin)
  climacs-syntax-common-lisp:cache
  ((%parent :initarg :parent :accessor clim:output-record-parent)))

(defclass common-lisp-view (clim-base:climacs-clim-view)
  ())

(defmethod clim-base:make-climacs-clim-view
    ((view climacs-syntax-common-lisp:view))
  (let* ((analyzer (climacs2-base:analyzer view))
         (cache (climacs-syntax-common-lisp:folio analyzer)))
    (make-instance 'common-lisp-view
      :output-history cache
      :climacs-view view)))

;;; Given a vector and a position, if the position NIL, meaning the
;;; end of the vector, then return the length of the vector.
;;; Otherwise, return the original position.
(defun canonicalize-column-number (contents position)
  (if (null position)
      (length contents)
      position))

;;; Return the text-style width, height, and ascent of PANE as three
;;; values.
(defun text-style-dimensions (pane)
  (let ((text-style (clim:medium-text-style pane)))
    (values (clim:text-style-width text-style pane)
            (clim:text-style-height text-style pane)
            (clim:text-style-ascent text-style pane))))

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

(defun draw-right-arrow
    (pane gutter line-number ink)
  (multiple-value-bind (middle h1 h2)
      (arrow-y-coordinates pane line-number)
      (clim:draw-polygon*
       gutter
       (list 0 (+ middle h1) 6 (+ middle h1) 6 (+ middle h2) 12 middle
             6 (- middle h2) 6 (- middle h1) 0 (- middle h1))
       :closed t :filled t :ink ink)))

;;; Draw an interval of text from a single line.  Optimize by not
;;; drawing anything if the defined interval is empty.  END-COLUMN can
;;; be NIL which means the end of CONTENTS.
(defun draw-interval (pane line-number contents start-column end-column)
  (multiple-value-bind (width height ascent) (text-style-dimensions pane)
    (let* ((y (+ ascent (* line-number height)))
           (x (* start-column width))
           (clim-view (clim:stream-default-view pane))
           (climacs-view (clim-base:climacs-view clim-view))
           (cursor (climacs2-base:cursor climacs-view))
           (cursor-column-number (cluffer:cursor-position cursor))
           (canonicalized-end-column-number
             (canonicalize-column-number contents end-column)))
      (unless (= start-column canonicalized-end-column-number)
        (if (= (cluffer:line-number cursor) line-number)
            (cond ((<= cursor-column-number start-column)
                   (clim:draw-text* pane contents
                                    (+ x 5) y
                                    :start start-column
                                    :end end-column))
                  ((>= cursor-column-number end-column)
                   (clim:draw-text* pane contents
                                    x y
                                    :start start-column
                                    :end end-column))
                  (t
                   (draw-interval pane line-number contents
                                  start-column cursor-column-number)
                   (draw-interval pane line-number contents
                                  cursor-column-number end-column)))
            (clim:draw-text* pane contents
                             x y
                             :start start-column
                             :end end-column)))
      (when (= (cluffer:line-number cursor) line-number)
        (cond ((= cursor-column-number start-column)
               (clim:draw-rectangle* pane (1+ x) (- y height) (+ x 4) y
                                     :ink clim:+blue+))
              ((= canonicalized-end-column-number
                  cursor-column-number
                  (length contents))
               (let ((cursor-x (* cursor-column-number width)))
                 (clim:draw-rectangle* pane
                                       (1+ cursor-x) (- y height)
                                       (+ cursor-x 4) y
                                       :ink clim:+blue+))))))))

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
         (let ((contents (climacs-syntax-common-lisp:line-contents
                          cache start-line-number)))
           (declare (type string contents))
           (draw-interval pane
                          start-line-number
                          contents
                          start-column-number
                          end-column-number)))
        (t
         (let ((first (climacs-syntax-common-lisp:line-contents
                       cache start-line-number)))
           (declare (type string first))
           (draw-interval pane
                          start-line-number
                          first
                          start-column-number
                          (length first)))
         (let ((last (climacs-syntax-common-lisp:line-contents
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
                  = (climacs-syntax-common-lisp:line-contents
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
      (setf el end-line
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
    ((view  climacs-syntax-common-lisp:view))
  (clim:find-command-table 'common-lisp-table))

(stealth-mixin:define-stealth-mixin
    presentation
    (clim:standard-presentation)
  climacs-syntax-common-lisp:wad
  (;; When this parse result is an element of the PREFIX, this slot
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
   (%max-line-width-list :accessor max-line-width-list))
  (:default-initargs :single-box t :type t))

;;; For convenience, define methods on MAX-LINE-WIDTH-LIST on CONS and
;;; NULL so that MAX-LINE-WIDTH-LIST can be applied to the prefix or
;;; the suffix directly.
(defmethod max-line-width-list ((object null))
  0)

(defmethod max-line-width-list ((object CONS))
  (max-line-width-list (first object)))

(defgeneric draw-wad (wad start-ref pane cache first-line last-line))

(defmethod draw-wad :around (wad start-ref pane cache first-line last-line)
  (declare (ignore cache first-line last-line))
  (let ((indentation (climacs-syntax-common-lisp::indentation wad))
        (start-column (climacs-syntax-common-lisp::start-column wad)))
    (unless (or (null indentation) (= indentation start-column))
      (let ((gutter (second-climacs-clim-base::left-gutter pane)))
        (if (< indentation start-column)
            (draw-left-arrow pane gutter start-ref clim:+blue+)
            (draw-right-arrow pane gutter start-ref clim:+blue+)))))
  (call-next-method))

(defmethod draw-wad :around
    ((wad climacs-syntax-common-lisp:comment-wad)
     start-ref pane cache first-line last-line)
  (declare (ignore start-ref pane cache first-line last-line))
  (clim:with-drawing-options (pane :ink clim:+brown+)
    (call-next-method)))

(defmethod draw-wad :before
    ((wad climacs-syntax-common-lisp:eof-wad)
     start-ref pane cache first-line last-line)
  (declare (ignore first-line last-line))
  (let ((start-col (climacs-syntax-common-lisp:start-column wad))
        (end-col (climacs-syntax-common-lisp:end-column wad))
        (height (climacs-syntax-common-lisp:height wad)))
    (unless (and (zerop height) (= start-col end-col))
      (draw-rectangle pane start-ref start-col (1+ start-col) clim:+orange+))))

(defmethod draw-wad :around
    ((wad climacs-syntax-common-lisp:error-wad)
     start-ref pane cache first-line last-line)
  (declare (ignore start-ref pane cache first-line last-line))
  (clim:with-drawing-options (pane :ink clim:+red+)
    (call-next-method)))

(defun draw-non-token-wad (wad start-ref pane cache first-line last-line)
  (flet ((start-line (wad) (climacs-syntax-common-lisp:start-line wad))
         (start-column (wad) (climacs-syntax-common-lisp:start-column wad))
         (height (wad) (climacs-syntax-common-lisp:height wad))
         (end-column (wad) (climacs-syntax-common-lisp:end-column wad)))
    (let ((children (climacs-syntax-common-lisp:children wad))
          (prev-end-line start-ref)
          (prev-end-column (start-column wad)))
      (let ((ref start-ref))
        (loop for child in children
              for start-line = (start-line child)
              until (> (+ ref start-line) last-line)
              do (incf ref (start-line child))
                 (draw-filtered-area pane cache
                                     prev-end-line
                                     prev-end-column
                                     ref
                                     (start-column child)
                                     first-line last-line)
                 (draw-wad child ref pane cache first-line last-line)
                 (setf prev-end-line (+ ref (height child)))
                 (setf prev-end-column (end-column child)))
        (draw-filtered-area pane cache
                            prev-end-line
                            prev-end-column
                            (+ start-ref (height wad))
                            (end-column wad)
                            first-line last-line)))))

(defgeneric draw-token-wad (wad token start-ref pane cache first-line last-line))

(defmethod draw-wad ((wad presentation)
                     start-ref
                     pane
                     (cache output-history)
                     first-line
                     last-line)
  (draw-non-token-wad wad start-ref pane cache first-line last-line))

(defmethod draw-wad
    ((wad climacs-syntax-common-lisp:expression-wad)
     start-ref pane (cache output-history) first-line last-line)
  (let ((expression (climacs-syntax-common-lisp:expression wad)))
    (if (typep expression 'climacs-syntax-common-lisp:token)
        (draw-token-wad
         wad expression start-ref pane cache first-line last-line)
        (draw-non-token-wad
         wad start-ref pane cache first-line last-line))))

;;; Given a folio and an interval of lines, return the maxium length
;;; of any lines in the interval.
(defun max-line-length (folio first-line-number last-line-number)
  (loop for line-number from first-line-number to last-line-number
        maximize (climacs-syntax-common-lisp:line-length folio line-number)))

(defmethod climacs-syntax-common-lisp:push-to-prefix :before
    (cache (wad presentation))
  (with-accessors ((max-line-width-list max-line-width-list)
                   (start-line climacs-syntax-common-lisp:start-line)
                   (max-line-width climacs-syntax-common-lisp:max-line-width))
      wad
    (with-accessors ((prefix climacs-syntax-common-lisp:prefix)) cache
      (setf max-line-width-list
            (if (null prefix)
                (max (max-line-length cache 0 (1- start-line))
                     max-line-width)
                (let* ((first (first prefix))
                       (end-line (climacs-syntax-common-lisp:end-line first)))
                  (max (max-line-width-list first)
                       (max-line-length cache (1+ end-line) (1- start-line))
                       max-line-width)))))))

(defmethod climacs-syntax-common-lisp:push-to-suffix :before
    (cache (wad presentation))
  (with-accessors ((max-line-width-list max-line-width-list)
                   (end-line climacs-syntax-common-lisp:end-line)
                   (max-line-width climacs-syntax-common-lisp:max-line-width))
      wad
    (with-accessors ((suffix climacs-syntax-common-lisp:suffix)) cache
      (setf max-line-width-list
            (if (null suffix)
                (max (max-line-length
                      cache
                      (1+ end-line)
                      (1- (climacs-syntax-common-lisp:line-count cache)))
                     max-line-width)
                (let* ((first (first suffix))
                       (start-line (climacs-syntax-common-lisp:start-line first)))
                  (max (max-line-width-list first)
                       (max-line-length cache (1+ end-line) (1- start-line))
                       max-line-width)))))))

(defun adjust-for-rendering (cache last-line)
  (with-accessors ((prefix climacs-syntax-common-lisp:prefix)
                   (suffix climacs-syntax-common-lisp:suffix))
      cache
    (loop until (or (null suffix)
                    (> (climacs-syntax-common-lisp:start-line (first suffix))
                       last-line))
          do (climacs-syntax-common-lisp:suffix-to-prefix cache))
    (loop until (or (null prefix)
                    (<= (climacs-syntax-common-lisp:start-line (first prefix))
                        last-line))
          do (climacs-syntax-common-lisp:prefix-to-suffix cache))))

(defgeneric render-empty-cache (cache pane first-line last-line))

(defmethod render-empty-cache ((cache output-history) pane first-line last-line)
  (let* ((line-count (climacs-syntax-common-lisp:line-count cache))
         (last-line-contents (climacs-syntax-common-lisp:line-contents
                              cache (1- line-count)))
         (end-column-number (length last-line-contents)))
    (draw-filtered-area pane cache
                        0 0
                        (1- line-count) end-column-number
                        first-line last-line)))

;;; When this function is called, the suffix contains no parse result
;;; to be rendered.  Either the suffix is empty, or the first parse
;;; result on the suffix lies entirely below the visible area.
;;; However, there may be some whitespace between the end of the first
;;; parse result on the prefix and either the first parse result on
;;; the suffix or the end of the buffer.  This function is responsible
;;; for rendering that whitespace.
(defun render-trailing-whitespace (cache pane first-line last-line)
  (with-accessors ((prefix climacs-syntax-common-lisp:prefix)
                   (suffix climacs-syntax-common-lisp:suffix)
                   (line-count climacs-syntax-common-lisp:line-count))
      cache
    (unless (null prefix)
      (let ((start-line
              (climacs-syntax-common-lisp:end-line (first prefix)))
            (start-column
              (climacs-syntax-common-lisp:end-column (first prefix)))
            (end-line
              (if (null suffix)
                  (1- line-count)
                  (climacs-syntax-common-lisp:start-line (first suffix))))
            (end-column
              (if (null suffix)
                  (climacs-syntax-common-lisp:line-length cache (1- line-count))
                  (climacs-syntax-common-lisp:start-column (first suffix)))))
        (draw-filtered-area pane cache
                            start-line start-column
                            end-line end-column
                            first-line last-line)))))

;;; Render the space between two consecutive top-level wads (WAD1 and
;;; WAD2) or (when WAD1 is NIL) between the beginning of the buffer and
;;; WAD2.
(defun render-gap (cache pane wad1 wad2 first-line last-line)
  (let ((start-line
          (if (null wad1) 0 (climacs-syntax-common-lisp:end-line wad1)))
        (start-column
          (if (null wad1) 0 (climacs-syntax-common-lisp:end-column wad1)))
        (end-line (climacs-syntax-common-lisp:start-line wad2))
        (end-column (climacs-syntax-common-lisp:start-column wad2)))
    (draw-filtered-area pane cache
                        start-line start-column
                        end-line end-column
                        first-line last-line)))

(defgeneric render-cache (cache pane first-line last-line))

(defmethod render-cache ((cache output-history) pane first-line last-line)
  (if (and (null (climacs-syntax-common-lisp:prefix cache))
           (null (climacs-syntax-common-lisp:suffix cache)))
      (render-empty-cache cache pane first-line last-line)
      (progn (adjust-for-rendering cache last-line)
             (render-trailing-whitespace cache pane first-line last-line)
             (loop with prefix = (climacs-syntax-common-lisp:prefix cache)
                   for (wad2 wad1) on prefix
                   until (< (climacs-syntax-common-lisp:end-line wad2)
                            first-line)
                   do (draw-wad
                       wad2
                       (climacs-syntax-common-lisp:start-line wad2)
                       pane
                       cache
                       first-line
                       last-line)
                      (render-gap cache pane wad1 wad2 first-line last-line)))))

;;; Return the area of the viewport of PANE in units of line and
;;; column number.  We return only integers, so that if a fraction of
;;; a line or a column is included in the viewport, then the entire
;;; line or column is included in the return values.  Four values are
;;; returned: The column and the line of the upper-left corner and the
;;; column and the line of the lower-right corner.
(defun viewport-area (pane)
  (multiple-value-bind (left top right bottom)
      (clim:bounding-rectangle*
       (clim:pane-viewport-region (clim:sheet-parent pane)))
    (multiple-value-bind (width height) (text-style-dimensions pane)
      (values (floor left width) (floor top height)
              (ceiling right width) (ceiling bottom height)))))

(defun clear-viewport (pane)
  (multiple-value-bind (left top right bottom)
      (clim:bounding-rectangle*
       (clim:pane-viewport-region (clim:sheet-parent pane)))
    (clim:medium-clear-area (clim:sheet-medium pane)
                            left top right bottom)))

(defmethod clim:replay-output-record
    ((cache output-history) stream &optional region x-offset y-offset)
  (declare (ignore x-offset y-offset region))
  (clear-viewport stream)
  (multiple-value-bind (left top right bottom)
      (viewport-area stream)
    (declare (ignore left right))
    (let* ((line-count (climacs-syntax-common-lisp:line-count cache))
           (last-line-number (min bottom (1- line-count))))
      (unless (minusp last-line-number)
        (render-cache cache stream top last-line-number)))))

(defun gap-start (history)
  (let ((prefix (climacs-syntax-common-lisp:prefix history)))
    (if (null prefix)
        0
        (1+ (climacs-syntax-common-lisp:end-line (first prefix))))))

(defun gap-end (history)
  (let ((suffix (climacs-syntax-common-lisp:suffix history))
        (line-count (climacs-syntax-common-lisp:line-count history)))
    (if (null suffix)
        (1- line-count)
        (1- (climacs-syntax-common-lisp:start-line (first suffix))))))

(defmethod clim:bounding-rectangle* ((history output-history))
  (let ((pane (clim:output-record-parent history)))
    (multiple-value-bind (width height) (text-style-dimensions pane)
      (let* ((line-count (climacs-syntax-common-lisp:line-count history))
             (prefix (climacs-syntax-common-lisp:prefix history))
             (gap-start (gap-start history))
             (suffix (climacs-syntax-common-lisp:suffix history))
             (gap-end (gap-end history))
             (total-width (max (max-line-width-list prefix)
                               (max-line-length history gap-start gap-end)
                               (max-line-width-list suffix))))
        (values 0 0 (* width total-width) (* height line-count))))))

;;; I don't know why this one is called at all
(defmethod clim:clear-output-record ((history output-history))
  nil)

(defmethod clim-base:update-view (pane (view common-lisp-view))
  (let ((history (clim:stream-output-history pane)))
    (climacs2-base:update-view (clim-base:climacs-view view))
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
  (declare (ignore x-offset y-offset function-args))
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
     (pane clim-base:text-pane)
     (analyzer climacs-syntax-common-lisp:analyzer))
  (update-cache view pane analyzer))
