(cl:in-package #:second-climacs-clim-view-common-lisp)

(defgeneric draw-wad (context wad start-ref cache))

(defgeneric draw-token-wad (context wad token start-ref cache))

(defgeneric draw-compound-wad (context wad start-ref cache))

;;; General wads

(defmethod draw-wad (context (wad ip:wad) start-ref (cache ip:cache))
  (draw-compound-wad context wad start-ref cache))

(defmethod draw-wad (context (wad ip:cst-wad) start-ref (cache ip:cache))
  ;; TODO doesn't work completely right. Disabled for now
  #+(or) (cl-syntax::compute-form-indentation wad nil nil)
  (let ((expression (cst:raw wad)))
    (if (typep expression 'ip:token)
        (draw-token-wad context wad expression start-ref cache)
        (draw-compound-wad context wad start-ref cache))))

(defmethod draw-wad :around (context (wad ip:comment-wad) start-ref cache)
  (declare (ignore start-ref cache))
  (let ((stream (stream* context)))
    (clim:with-drawing-options (stream :ink clim:+brown+)
      (call-next-method))))

(defmethod draw-wad :around (context (wad ip:ignored-wad) start-ref cache)
  (declare (ignore start-ref cache))
  (let ((stream (stream* context)))
    (clim:with-drawing-options (stream :ink clim:+gray50+)
      (call-next-method))))

(flet ((draw-underline (context wad ink thickness dashes)
         (let* ((start-line   (ip:absolute-start-line wad))
                (start-column (ip:start-column wad))
                (end-line     (+ start-line (ip:height wad)))
                (end-column   (ip:end-column wad))
                (line-height  (line-height context))
                (thickness    (* thickness line-height)))
           (draw-underline-marker
            context start-line start-column end-line end-column
            :ink            ink
            :line-thickness thickness
            :line-dashes    dashes))))

  (defmethod draw-wad :after (context (wad ip:word-wad) start-ref cache)
    (declare (ignore cache))
    (when (ip:misspelled wad)
      (let ((dash-length (* 1/6 (line-height context))))
        (draw-underline context wad clim:+orange+ 1/8
                        (list (* 2 dash-length)
                              (* 2 dash-length))))))

  (defmethod draw-wad :after (context (wad ip:labeled-object-definition-wad)
                              start-ref cache)
    (declare (ignore cache))
    (draw-underline context wad clim:+foreground-ink+ 1/12 nil))

  (defmethod draw-wad :after (context (wad ip:labeled-object-reference-wad)
                              start-ref cache)
    (declare (ignore cache))
    (draw-underline context wad clim:+foreground-ink+ 1/12 nil)))

;;; Token wads

(defmethod draw-token-wad :around
    (context wad (token ip:existing-symbol-token) start-ref cache)
  (declare (ignore wad start-ref cache))
  (if (equal (ip:package-name token) "COMMON-LISP")
      (let ((stream (stream* context)))
        (clim:with-drawing-options (stream :ink clim:+purple+)
          (call-next-method)))
      (call-next-method)))

(flet ((draw-rectangles (context
                         line start-column middle-column end-column
                         package-ink symbol-ink)
         (draw-one-line-rectangle context line start-column middle-column
                                  :include-descent t
                                  :ink             package-ink)
         (draw-one-line-rectangle context line middle-column end-column
                                  :include-descent t
                                  :ink             symbol-ink)))

  (defmethod draw-token-wad :before
      (context wad (token ip:non-existing-symbol-token) start-ref cache)
    (declare (ignore cache))
    (let ((pos (ip:package-marker-1 token))
          (start (ip:start-column wad))
          (end (ip:end-column wad))
          (height (ip:height wad)))
      (unless (or (null pos)
                  (not (zerop height))
                  (zerop pos))
        (draw-rectangles context start-ref start (+ start pos 1) end clim:+pink+ clim:+red+))))

  (defmethod draw-token-wad :before
      (context wad (token ip:non-existing-package-symbol-token)
       start-ref cache)
    (declare (ignore cache))
    (let ((pos (ip:package-marker-1 token))
          (start (ip:start-column wad))
          (end (ip:end-column wad))
          (height (ip:height wad)))
      (unless (or (null pos) (not (zerop height)))
        (draw-rectangles context start-ref start (+ start pos) end clim:+red+ clim:+pink+)))))

(defmethod draw-token-wad (context wad token start-ref (cache ip:cache))
  ;; Call DRAW-COMPOUND-WAD instead of DRAW-FILTERED-AREA in case WAD
  ;; has children.
  (draw-compound-wad context wad start-ref cache))

;;; Compound wads

(defmethod draw-compound-wad :around (context (wad ip:cst-wad) start-ref cache)
  (typecase (cst:raw wad)
    (number (let ((stream (stream* context)))
              (clim:with-drawing-options (stream :ink clim:+dark-blue+)
                (call-next-method))))
    (string (let ((stream (stream* context)))
              (clim:with-drawing-options (stream :ink clim:+dark-goldenrod+)
                (call-next-method))))
    (t      (call-next-method))))

(defmethod draw-compound-wad (context wad start-ref cache)
  (let ((min-line        (min-line context))
        (min-column      (min-column context))
        (max-line        (max-line context))
        (children        (ip:children wad))
        (prev-end-line   start-ref)
        (prev-end-column (ip:start-column wad)))
    (loop for child      in children
          for start-line =  (ip:absolute-start-line child)
          for height     =  (ip:height child)
          until (> start-line max-line)
          do ;; Ensure that only at least partially visible wads are
             ;; passed to DRAW-FILTERED-AREA and DRAW-WAD.
             (when (or ;; Start visible.
                       (<= min-line start-line max-line)
                       ;; End visible.
                       (<= min-line (+ start-line height) max-line)
                       ;; Contains visible region.
                       (<= start-line min-line max-line (+ start-line height)))
               (assert (or      (< prev-end-line   start-line)
                           (and (= prev-end-line   start-line)
                                (<= prev-end-column (ip:start-column child)))))
               (draw-filtered-area context cache
                                   prev-end-line
                                   prev-end-column
                                   start-line
                                   (ip:start-column child)
                                   min-line min-column max-line nil)
               (draw-wad context child start-line cache))
             (setf prev-end-line   (+ start-line height)
                   prev-end-column (ip:end-column child)))
    (draw-filtered-area context cache
                        prev-end-line
                        prev-end-column
                        (+ start-ref (ip:height wad))
                        (ip:end-column wad)
                        min-line min-column max-line nil)))

(defun wad-is-incomplete-p (wad)
  (some (lambda (error)
          (typep (ip:condition error) 'eclector.reader:missing-delimiter))
        (ip:errors wad)))

(defmethod draw-compound-wad :after (context (wad ip:wad) start-ref cache)
  ;; TODO remember point and mark lines in context to avoid the repeated lookups
  (let* ((pane                 (stream* context))
         (clim-view            (clim:stream-default-view pane))
         (climacs-view         (clim-base:climacs-view clim-view))
         (analyzer             (base:analyzer climacs-view))
         (buffer               (ip:buffer analyzer))
         (cursor               (edit:point buffer))
         (cursor-line-number   nil) ; expensive to compute
         (cursor-column-number (cluffer:cursor-position cursor))
         (wad-start-line       start-ref)
         (wad-start-column     (ip:start-column wad))
         (wad-end-line         (+ wad-start-line (ip:height wad)))
         (wad-end-column       (ip:end-column wad)))
    (when (and (typecase wad
                 (ip:cst-wad
                  (typep (cst:raw wad) '(or cons string vector)))
                 ((or ip:block-comment-wad ip:reader-macro-wad)
                  t))
               (or (and (= wad-end-column   cursor-column-number)
                        (= wad-end-line     (setf cursor-line-number
                                                  (cluffer:line-number
                                                   cursor))))
                   (and (= wad-start-column cursor-column-number)
                        (= wad-start-line   (or cursor-line-number
                                                (setf cursor-line-number
                                                      (cluffer:line-number
                                                       cursor)))))))
      ;; `wad-is-incomplete-p' is expensive.
      (let* ((completep (not (wad-is-incomplete-p wad)))
             (ink       (if completep
                            clim:+green+
                            clim:+red+)))
        (clim:with-drawing-options (pane :text-face :bold :ink ink)
          ;; Highlight opening delimiter.
          (draw-area context cache
                     wad-start-line wad-start-column
                     wad-start-line (1+ wad-start-column))
          ;; Highlight closing delimiter if complete.
          (when (and completep (plusp wad-end-column))
            (draw-area context cache
                       wad-end-line (1- wad-end-column)
                       wad-end-line wad-end-column)))))))

;;; Buffer content drawing

(defun draw-filtered-area (context cache
                           start-line start-column end-line end-column
                           min-line   min-column   max-line max-column)
  (multiple-value-call #'draw-area context cache
    (filter-area start-line start-column end-line end-column
                 min-line   min-column   max-line max-column)))

;;; Draw an area of text defined by START-LINE, START-COLUMN,
;;; END-LINE, and END-COLUMN.  The text is drawn on PANE, using the
;;; contents from CACHE.  END-COLUMN is permitted to be NIL, meaning
;;; the end of the line designated by END-LINE.
(defun draw-area (context cache start-line start-column end-line end-column)
  (cond ((= start-line end-line)
         (let ((contents (ip:line-contents cache start-line)))
           (declare (type string contents))
           (draw-interval
            context start-line contents start-column end-column)))
        (t
         (let ((first (ip:line-contents cache start-line)))
           (declare (type string first))
           (draw-interval
            context start-line first start-column (length first)))
         (loop for line from (1+ start-line) to (1- end-line)
               for contents of-type string
                  = (ip:line-contents cache line)
               do (draw-interval context line contents 0 (length contents)))
         (let ((last (ip:line-contents cache end-line)))
           (declare (type string last))
           (draw-interval context end-line last 0 end-column)))))

;;; Draw an interval of text from a single line.  Optimize by not
;;; drawing anything if the defined interval is empty.  END-COLUMN can
;;; be NIL which means the end of CONTENTS.
(defun draw-interval (context line-number contents start-column end-column)
  (when (< start-column (if (null end-column)
                            (length contents)
                            end-column))
    (multiple-value-bind (x y)
        (text-position context line-number start-column :include-ascent t)
      (clim:draw-text* (stream* context) contents x y :start start-column
                                                      :end   end-column))))
