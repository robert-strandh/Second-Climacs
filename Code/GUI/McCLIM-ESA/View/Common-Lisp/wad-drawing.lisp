(cl:in-package #:second-climacs-clim-view-common-lisp)

(defgeneric draw-wad (wad start-ref pane cache first-line last-line))

(defgeneric draw-token-wad (wad token start-ref pane cache first-line last-line))

(defgeneric draw-non-token-wad (wad start-ref pane cache first-line last-line))

;;; General wads

(defmethod draw-wad ((wad ip:expression-wad)
                     start-ref
                     pane
                     (cache output-history)
                     first-line
                     last-line)
  (cl-syntax::compute-form-indentation wad nil nil)
  (let ((expression (ip:expression wad)))
    (if (typep expression 'ip:token)
        (draw-token-wad
         wad expression start-ref pane cache first-line last-line)
        (draw-non-token-wad
         wad start-ref pane cache first-line last-line))))

(defmethod draw-wad ((wad presentation)
                     start-ref
                     pane
                     (cache output-history)
                     first-line
                     last-line)
  (draw-non-token-wad wad start-ref pane cache first-line last-line))

(defmethod draw-wad :around ((wad ip:comment-wad)
                             start-ref pane cache first-line last-line)
  (declare (ignore start-ref cache first-line last-line))
  (clim:with-drawing-options (pane :ink clim:+brown+)
    (call-next-method)))

(defmethod draw-wad :around ((wad ip:ignored-wad)
                             start-ref pane cache first-line last-line)
  (declare (ignore start-ref cache first-line last-line))
  (clim:with-drawing-options (pane :ink clim:+gray50+)
    (call-next-method)))

(defmethod draw-wad :around ((wad ip:word-wad)
                             start-ref pane cache first-line last-line)
  (declare (ignore cache first-line last-line))
  (if (ip:misspelled wad)
      (clim:surrounding-output-with-border (pane :shape :underline
                                                 :ink            clim:+orange+
                                                 :padding        2
                                                 :line-thickness 2
                                                 :line-dashes    '(4 4))
        (call-next-method))
      (call-next-method)))

(flet ((draw-underline (pane line-number wad)
         (multiple-value-bind (style-width style-height ascent) (text-style-dimensions pane)
           (let* ((start-column (ip:start-column wad))
                  (end-column (ip:end-column wad))
                  (x (* start-column style-width))
                  (y (+ ascent (* line-number style-height)))
                  (width (* style-width (- end-column start-column))))
             (clim:draw-line* pane x (+ y 2) (+ x width) (+ y 2))))))

  (defmethod draw-wad :around ((wad ip:labeled-object-definition-wad)
                               start-ref pane cache first-line last-line)
    (declare (ignore cache first-line last-line))
    (call-next-method)
    (draw-underline pane start-ref wad))

  (defmethod draw-wad :around ((wad ip:labeled-object-reference-wad)
                               start-ref pane cache first-line last-line)
    (declare (ignore cache first-line last-line))
    (call-next-method)
    (draw-underline pane start-ref wad)))

;;; Token wads

(defmethod draw-token-wad :around (wad (token ip:numeric-token)
                                   start-ref pane cache first-line last-line)
  (declare (ignore wad start-ref cache first-line last-line))
  (clim:with-drawing-options (pane :ink clim:+dark-blue+)
    (call-next-method)))

(defmethod draw-token-wad :around
    (wad (token ip:existing-symbol-token)
     start-ref pane cache first-line last-line)
  (declare (ignore wad start-ref cache first-line last-line))
  (if (equal (ip:package-name token) "COMMON-LISP")
      (clim:with-drawing-options (pane :ink clim:+purple+)
        (call-next-method))
      (call-next-method)))

(defmethod draw-token-wad :before
    (wad (token ip:non-existing-symbol-token)
     start-ref pane cache first-line last-line)
  (declare (ignore cache first-line last-line))
  (let ((pos (ip:package-marker-1 token))
        (start (ip:start-column wad))
        (end (ip:end-column wad))
        (height (ip:height wad)))
    (unless (or (null pos)
                (not (zerop height))
                (zerop pos))
      (draw-rectangle pane start-ref start (+ start pos 1) clim:+pink+)
      (draw-rectangle pane start-ref (+ start pos 1) end clim:+red+))))

(defmethod draw-token-wad :before
    (wad (token ip:non-existing-package-symbol-token)
     start-ref pane cache first-line last-line)
  (declare (ignore cache first-line last-line))
  (let ((pos (ip:package-marker-1 token))
        (start (ip:start-column wad))
        (end (ip:end-column wad))
        (height (ip:height wad)))
    (unless (or (null pos) (not (zerop height)))
      (draw-rectangle pane start-ref start (+ start pos) clim:+red+)
      (draw-rectangle pane start-ref (+ start pos) end clim:+pink+))))

(defmethod draw-token-wad
    (wad token start-ref pane (cache output-history) first-line last-line)
  ;; Call DRAW-NON-TOKEN-WAD (the name should probably change) instead
  ;; of DRAW-FILTERED-AREA in case WAD has children.
  (draw-non-token-wad wad start-ref pane cache first-line last-line))

;;; Non-token wads

(defmethod draw-non-token-wad :around ((wad ip:expression-wad)
                                       start-ref pane cache first-line last-line)
  (typecase (ip:expression wad)
    (string (clim:with-drawing-options (pane :ink clim:+dark-goldenrod+)
              (call-next-method)))
    (t      (call-next-method))))

(defmethod draw-non-token-wad (wad start-ref pane cache first-line last-line)
  (flet ((start-column (wad) (ip:start-column wad))
         (height (wad) (ip:height wad))
         (end-column (wad) (ip:end-column wad)))
    (let ((children (ip:children wad))
          (prev-end-line start-ref)
          (prev-end-column (start-column wad)))
      (loop for child in children
            for height     = (height child)
            until (> (ip:absolute-start-line-number child) last-line)
            do ;; Ensure that only at least partially visible wads are
               ;; passed to DRAW-FILTERED-AREA and DRAW-WAD.
               (when (or
                      ;; Start visible.
                      (<= first-line
                          (ip:absolute-start-line-number child)
                          last-line)
                      ;; End visible.
                      (<= first-line
                          (+ (ip:absolute-start-line-number child) height)
                          last-line)
                      ;; Contains visible region.
                      (<= (ip:absolute-start-line-number child)
                          first-line
                          last-line
                          (+ (ip:absolute-start-line-number child) height)))
                 (draw-filtered-area pane cache
                                     prev-end-line
                                     prev-end-column
                                     (ip:absolute-start-line-number child)
                                     (start-column child)
                                     first-line last-line)
                 (draw-wad child (ip:absolute-start-line-number child) pane cache first-line last-line))
               (setf prev-end-line   (+ (ip:absolute-start-line-number child) height)
                     prev-end-column (end-column child)))
      (draw-filtered-area pane cache
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

(defmethod draw-non-token-wad :after
    ((wad ip:expression-wad) start-ref pane cache first-line last-line)
  (let* ((expression (ip:expression wad))
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
            (draw-area pane cache
                       start-ref wad-start-column
                       start-ref (1+ wad-start-column))))))))
