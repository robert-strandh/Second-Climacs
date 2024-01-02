(cl:in-package #:second-climacs-clim-view-common-lisp)

(stealth-mixin:define-stealth-mixin
    presentation
    (clim:standard-presentation)
  ip:wad
  ()
  (:default-initargs :single-box t :type t))

(defgeneric draw-wad (context wad start-ref cache first-line last-line))

(defgeneric draw-token-wad (context wad token start-ref cache first-line last-line))

(defgeneric draw-compound-wad (context wad start-ref cache first-line last-line))

;;; General wads

(defmethod draw-wad (context
                     (wad presentation)
                     start-ref
                     (cache ip:cache)
                     first-line
                     last-line)
  (draw-compound-wad context wad start-ref cache first-line last-line))

(defmethod draw-wad (context
                     (wad ip:expression-wad)
                     start-ref
                     (cache ip:cache)
                     first-line
                     last-line)
  (cl-syntax::compute-form-indentation wad nil nil)
  (let ((expression (ip:expression wad)))
    (if (typep expression 'ip:token)
        (draw-token-wad
         context wad expression start-ref cache first-line last-line)
        (draw-compound-wad
         context wad start-ref cache first-line last-line))))

(defmethod draw-wad :around (context (wad ip:comment-wad)
                             start-ref cache first-line last-line)
  (declare (ignore start-ref cache first-line last-line))
  (let ((stream (stream* context)))
    (clim:with-drawing-options (stream :ink clim:+brown+)
      (call-next-method))))

(defmethod draw-wad :around (context (wad ip:ignored-wad)
                             start-ref cache first-line last-line)
  (declare (ignore start-ref cache first-line last-line))
  (let ((stream (stream* context)))
    (clim:with-drawing-options (stream :ink clim:+gray50+)
      (call-next-method))))

(flet ((draw-underline (context line-number wad ink thickness)
         (let* ((line-height     (line-height context))
                (character-width (character-width context))
                (start-column    (ip:start-column wad))
                (end-column      (ip:end-column wad)))
           (multiple-value-bind (x y)
               (text-position context line-number start-column :include-ascent t)
             (let ((width        (* character-width (- end-column start-column)))
                   (thickness    (* thickness line-height)))
               (clim:draw-line* (stream* context) x (+ y 2) (+ x width) (+ y 2)
                                :ink            ink
                                :line-thickness thickness))))))

  (defmethod draw-wad :after (context (wad ip:labeled-object-definition-wad)
                              start-ref cache first-line last-line)
    (declare (ignore cache first-line last-line))
    (draw-underline context start-ref wad clim:+foreground-ink+ 1/12))

  (defmethod draw-wad :after (context (wad ip:labeled-object-reference-wad)
                              start-ref cache first-line last-line)
    (declare (ignore cache first-line last-line))
    (draw-underline context start-ref wad clim:+foreground-ink+ 1/12)))

;;; Token wads

(defmethod draw-token-wad :around
    (context wad (token ip:numeric-token)
     start-ref cache first-line last-line)
  (declare (ignore wad start-ref cache first-line last-line))
  (let ((stream (stream* context)))
    (clim:with-drawing-options (stream :ink clim:+dark-blue+)
      (call-next-method))))

(defmethod draw-token-wad :around
    (context wad (token ip:existing-symbol-token)
     start-ref cache first-line last-line)
  (declare (ignore wad start-ref cache first-line last-line))
  (if (equal (ip:package-name token) "COMMON-LISP")
      (let ((stream (stream* context)))
        (clim:with-drawing-options (stream :ink clim:+purple+)
          (call-next-method)))
      (call-next-method)))

(flet ((draw-rectangles (context
                         line start-column middle-column end-column
                         package-ink symbol-ink)
         (draw-one-line-rectangle context line start-column middle-column
                                  :ink package-ink)
         (draw-one-line-rectangle context line middle-column end-column
                                  :ink symbol-ink)))

  (defmethod draw-token-wad :before
      (context wad (token ip:non-existing-symbol-token)
       start-ref cache first-line last-line)
    (declare (ignore cache first-line last-line))
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
       start-ref cache first-line last-line)
    (declare (ignore cache first-line last-line))
    (let ((pos (ip:package-marker-1 token))
          (start (ip:start-column wad))
          (end (ip:end-column wad))
          (height (ip:height wad)))
      (unless (or (null pos) (not (zerop height)))
        (draw-rectangles context start-ref start (+ start pos) end clim:+red+ clim:+pink+)))))

(defmethod draw-token-wad
    (context wad token start-ref (cache ip:cache) first-line last-line)
  ;; Call DRAW-COMPOUND-WAD instead of DRAW-FILTERED-AREA in case WAD
  ;; has children.
  (draw-compound-wad context wad start-ref cache first-line last-line))

;;; Compound wads

(defmethod draw-compound-wad :around (context (wad ip:expression-wad)
                                      start-ref cache first-line last-line)
  (typecase (ip:expression wad)
    (string (let ((stream (stream* context)))
              (clim:with-drawing-options (stream :ink clim:+dark-goldenrod+)
                (call-next-method))))
    (t      (call-next-method))))

(defmethod draw-compound-wad (context wad start-ref cache first-line last-line)
  (flet ((start-column (wad) (ip:start-column wad))
         (height (wad) (ip:height wad))
         (end-column (wad) (ip:end-column wad)))
    (let ((children (ip:children wad))
          (prev-end-line start-ref)
          (prev-end-column (start-column wad)))
      (loop for child             in children
            for start-line-number =  (ip:absolute-start-line-number child)
            for height            =  (height child)
            until (> start-line-number last-line)
            do ;; Ensure that only at least partially visible wads are
               ;; passed to DRAW-FILTERED-AREA and DRAW-WAD.
               (when (or ;; Start visible.
                         (<= first-line start-line-number last-line)
                         ;; End visible.
                         (<= first-line
                             (+ start-line-number height)
                             last-line)
                         ;; Contains visible region.
                         (<= start-line-number
                             first-line
                             last-line
                             (+ start-line-number height)))
                 (draw-filtered-area context cache
                                     prev-end-line
                                     prev-end-column
                                     start-line-number
                                     (start-column child)
                                     first-line last-line)
                 (draw-wad context child start-line-number cache first-line last-line))
               (setf prev-end-line   (+ start-line-number height)
                     prev-end-column (end-column child)))
      (draw-filtered-area context cache
                          prev-end-line
                          prev-end-column
                          (+ start-ref (height wad))
                          (end-column wad)
                          first-line last-line))))

(defun wad-is-incomplete-p (wad)
  (loop for child in (ip:children wad)
          thereis (or (and (typep child 'ip:error-wad)
                           (typep (ip::condition* child)
                                  'eclector.reader:unterminated-list))
                      (wad-is-incomplete-p child))))

(defmethod draw-compound-wad :after
    (context (wad ip:expression-wad) start-ref cache first-line last-line)
  (let* ((expression (ip:expression wad))
         (pane (stream* context))
         (clim-view (clim:stream-default-view pane))
         (climacs-view (clim-base:climacs-view clim-view))
         (cursor (base:cursor climacs-view))
         (cursor-line-number (cluffer:line-number cursor))
         (cursor-column-number (cluffer:cursor-position cursor)))
    (when (and (consp expression)
               (= (+ start-ref (ip:height wad)) cursor-line-number)
               (= (ip:end-column wad) cursor-column-number))
      (unless (wad-is-incomplete-p wad)
        (let ((wad-start-column (ip:start-column wad)))
          (clim:with-text-face (pane :bold)
            (draw-area context cache
                       start-ref wad-start-column
                       start-ref (1+ wad-start-column))))))))

;;; Buffer content drawing

(defun draw-filtered-area (context cache
                           start-line-number start-column-number
                           end-line-number end-column-number
                           first-line last-line)
  (multiple-value-call #'draw-area context cache
    (filter-area start-line-number start-column-number
                 end-line-number end-column-number
                 first-line last-line)))

;;; Draw an area of text defined by START-LINE, START-COLUMN,
;;; END-LINE-NUMBER, and END-COLUMN.  The text is drawn on PANE, using
;;; the contents from CACHE.  END-COLUMN is permitted to be NIL,
;;; meaning the end of the line designated by END-LINE.
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
               do (draw-interval
                   context line contents 0 (length contents)))
         (let ((last (ip:line-contents cache end-line)))
           (declare (type string last))
           (draw-interval context end-line last 0 end-column)))))

;;; Draw an interval of text from a single line.  Optimize by not
;;; drawing anything if the defined interval is empty.  END-COLUMN can
;;; be NIL which means the end of CONTENTS.
(defun draw-interval (context line-number contents start-column end-column)
  (unless (= start-column (if (null end-column)
                              (length contents)
                              end-column))
    (multiple-value-bind (x y)
        (text-position context line-number start-column :include-ascent t)
      (clim:draw-text* (stream* context) contents x y :start start-column
                                                      :end end-column))))
