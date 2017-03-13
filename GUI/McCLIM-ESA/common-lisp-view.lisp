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

;;; Given a vector and a position, if the position NIL, meaning the
;;; end of the vector, then return the length of the vector.
;;; Otherwise, return the original position.
(defun canonicalize-column-number (contents position)
  (if (null position)
      (length contents)
      position))

;;; Given a line number, return the Y coordinate of the base line of
;;; the line with that number.
(defun base-line-position (text-style pane line-number)
  (let ((text-height (clim:text-style-height text-style pane))
        (text-ascent (clim:text-style-ascent text-style pane)))
    (+ text-ascent (* line-number text-height))))

;;; Draw an interval of text from a single line.  Optimize by not
;;; drawing anything if the defined interval is empty.  END-COLUMN can
;;; be NIL which means the end of CONTENTS.
(defun draw-interval (pane line-number contents start-column end-column)
  (let* ((text-style (clim:medium-text-style pane))
         (text-height (clim:text-style-height text-style pane))
         (text-width (clim:text-style-width text-style pane))
         (text-ascent (clim:text-style-ascent text-style pane))
         (y (+ text-ascent (* line-number text-height)))
         (x (* start-column text-width)))
    (unless (= start-column (canonicalize-column-number contents end-column))
      (clim:draw-text* pane contents x y :start start-column :end end-column))))

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

(defmethod command-table
    ((view  climacs-syntax-common-lisp:view))
  (clim:find-command-table 'common-lisp-table))

(stealth-mixin:define-stealth-mixin
    presentation
    (clim:standard-presentation)
  climacs-syntax-common-lisp:parse-result
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
  (:default-initargs :single-box t))

(defgeneric draw-parse-result (parse-result
                               pane
                               cache
                               first-line
                               last-line))

(defmethod draw-parse-result ((parse-result presentation)
                              pane
                              (cache output-history)
                              first-line
                              last-line)
  (flet ((start-line (pr) (climacs-syntax-common-lisp:start-line pr))
         (start-column (pr) (climacs-syntax-common-lisp:start-column pr))
         (end-line (pr) (climacs-syntax-common-lisp:end-line pr))
         (end-column (pr) (climacs-syntax-common-lisp:end-column pr)))
    (let ((children (climacs-syntax-common-lisp:children parse-result))
          (pr parse-result))
      (if (null children)
          (draw-filtered-area pane cache
                              (start-line pr) (start-column pr)
                              (end-line pr) (end-column pr)
                              first-line last-line)
          (progn
            ;; Start by drawing the area preceding the first child.
            (draw-filtered-area pane cache
                                (start-line pr)
                                (start-column pr)
                                (start-line (first children))
                                (start-column (first children))
                                first-line last-line)
            ;; Next, for each child except the first, first draw the
            ;; area between the end of the preceding sibling and
            ;; beginning of the child, and then draw the child itself.
            (loop for sibling in children
                  for child in (rest children)
                  do (draw-filtered-area pane cache
                                         (end-line sibling)
                                         (end-column sibling)
                                         (start-line child)
                                         (start-column child)
                                         first-line last-line)
                     (draw-parse-result child pane cache
                                        first-line last-line))
            ;; Finally, draw the area between the end of the last child
            ;; and the end of this parse result.
            (let ((last-child (first (last children))))
              (draw-filtered-area pane cache
                                  (end-line last-child)
                                  (end-column last-child)
                                  (end-line pr)
                                  (end-column pr)
                                  first-line last-line)))))))

;;; Given a folio and an interval of lines, return the maxium length
;;; of any lines in the interval.
(defun max-line-length (folio first-line-number last-line-number)
  (loop for line-number from first-line-number to last-line-number
        maximize (climacs-syntax-common-lisp:line-length folio line-number)))

(defmethod climacs-syntax-common-lisp:push-to-prefix :before
    (cache (parse-result presentation))
  (with-accessors ((max-line-width-list max-line-width-list)
                   (start-line climacs-syntax-common-lisp:start-line)
                   (max-line-width climacs-syntax-common-lisp:max-line-width))
      parse-result
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
    (cache (parse-result presentation))
  (with-accessors ((max-line-width-list max-line-width-list)
                   (end-line climacs-syntax-common-lisp:end-line)
                   (max-line-width climacs-syntax-common-lisp:max-line-width))
      parse-result
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

(defun adjust-for-rendering (cache first-line last-line)
  (with-accessors ((prefix climacs-syntax-common-lisp:prefix)
                   (suffix climacs-syntax-common-lisp:suffix))
      cache
    (loop until (or (null suffix)
                    (> (climacs-syntax-common-lisp:start-line (first suffix))
                       last-line))
          do (climacs-syntax-common-lisp:suffix-to-prefix cache))
    (loop until (or (null prefix)
                    (>= (climacs-syntax-common-lisp:end-line (first prefix))
                        first-line))
          do (climacs-syntax-common-lisp:prefix-to-suffix cache))))

(defgeneric render-cache (cache pane first-line last-line))

(defmethod render-cache ((cache output-history) pane first-line last-line)
  (adjust-for-rendering cache first-line last-line)
  (loop with prefix = (climacs-syntax-common-lisp:prefix cache)
        for parse-result in prefix
        until (< (climacs-syntax-common-lisp:end-line parse-result)
                 first-line)
        do (draw-parse-result parse-result pane cache first-line last-line)))

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
         (text-style-width (clim:text-style-width text-style stream))
         (line-count (climacs-syntax-common-lisp:line-count history))
         (prefix (climacs-syntax-common-lisp:prefix history))
         (pfirst (first prefix))
         (gap-start (if (null prefix)
                        0
                        (1+ (climacs-syntax-common-lisp:end-line pfirst))))
         (suffix (climacs-syntax-common-lisp:suffix history))
         (sfirst (first suffix))
         (gap-end (if (null suffix)
                      (1- line-count)
                      (1- (climacs-syntax-common-lisp:start-line sfirst))))
         (width (max (if (null prefix)
                         0
                         (max-line-width-list pfirst))
                     (max-line-length history gap-start gap-end)
                     (if (null suffix)
                         0
                         (max-line-width-list sfirst)))))
    (values 0
            0
            (* text-style-width width)
            (* text-style-height line-count))))

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
     (pane text-pane)
     (analyzer climacs-syntax-common-lisp:analyzer))
  (update-cache view pane analyzer))
