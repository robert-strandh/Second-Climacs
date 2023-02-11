(cl:in-package #:second-climacs-clim-view-common-lisp)

(defgeneric draw-wad (wad start-ref pane cache first-line last-line))

(defgeneric draw-token-wad (wad token start-ref pane cache first-line last-line))

(defgeneric draw-non-token-wad (wad start-ref pane cache first-line last-line))

;;; General wads

(defmethod draw-wad ((wad cl-syntax:expression-wad)
                     start-ref
                     pane
                     (cache output-history)
                     first-line
                     last-line)
  (let ((expression (cl-syntax:expression wad)))
    (if (typep expression 'cl-syntax:token)
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

(defmethod draw-wad :before ((wad cl-syntax:eof-wad)
                             start-ref pane cache first-line last-line)
  (declare (ignore first-line last-line))
  (let ((start-col (cl-syntax:start-column wad))
        (end-col (cl-syntax:end-column wad))
        (height (cl-syntax:height wad)))
    (unless (and (zerop height) (= start-col end-col))
      (draw-rectangle pane start-ref start-col (1+ start-col) clim:+orange+))))

(defmethod draw-wad :around ((wad cl-syntax:error-wad)
                             start-ref pane cache first-line last-line)
  (declare (ignore start-ref cache first-line last-line))
  (clim:with-drawing-options (pane :ink clim:+red+)
    (call-next-method)))

(defmethod draw-wad :around ((wad cl-syntax:comment-wad)
                             start-ref pane cache first-line last-line)
  (declare (ignore start-ref cache first-line last-line))
  (clim:with-drawing-options (pane :ink clim:+brown+)
    (call-next-method)))

(defmethod draw-wad :around ((wad cl-syntax:ignored-wad)
                             start-ref pane cache first-line last-line)
  (declare (ignore start-ref cache first-line last-line))
  (clim:with-drawing-options (pane :ink clim:+gray50+)
    (call-next-method)))

;;; Token wads

(defmethod draw-token-wad :around (wad (token cl-syntax:numeric-token)
                                   start-ref pane cache first-line last-line)
  (declare (ignore wad start-ref cache first-line last-line))
  (clim:with-drawing-options (pane :ink clim:+dark-blue+)
    (call-next-method)))

(defmethod draw-token-wad :around
    (wad (token cl-syntax:existing-symbol-token)
     start-ref pane cache first-line last-line)
  (declare (ignore wad start-ref cache first-line last-line))
  (if (equal (cl-syntax:package-name token) "COMMON-LISP")
      (clim:with-drawing-options (pane :ink clim:+purple+)
        (call-next-method))
      (call-next-method)))

(defmethod draw-token-wad :before
    (wad (token cl-syntax:non-existing-symbol-token)
     start-ref pane cache first-line last-line)
  (declare (ignore cache first-line last-line))
  (let ((pos (cl-syntax:package-marker-1 token))
        (start (cl-syntax:start-column wad))
        (end (cl-syntax:end-column wad))
        (height (cl-syntax:height wad)))
    (unless (or (null pos)
                (not (zerop height))
                (zerop pos))
      (draw-rectangle pane start-ref start (+ start pos 1) clim:+pink+)
      (draw-rectangle pane start-ref (+ start pos 1) end clim:+red+))))

(defmethod draw-token-wad :before
    (wad (token cl-syntax:non-existing-package-symbol-token)
     start-ref pane cache first-line last-line)
  (declare (ignore cache first-line last-line))
  (let ((pos (cl-syntax:package-marker-1 token))
        (start (cl-syntax:start-column wad))
        (end (cl-syntax:end-column wad))
        (height (cl-syntax:height wad)))
    (unless (or (null pos) (not (zerop height)))
      (draw-rectangle pane start-ref start (+ start pos) clim:+red+)
      (draw-rectangle pane start-ref (+ start pos) end clim:+pink+))))

(defmethod draw-token-wad
    (wad token start-ref pane (cache output-history) first-line last-line)
  (let* ((start-column (cl-syntax:start-column wad))
         (end-column (cl-syntax:end-column wad))
         (height (cl-syntax:height wad)))
    (draw-filtered-area pane cache
                        start-ref
                        start-column
                        (+ start-ref height)
                        end-column
                        first-line last-line)))

;;; Non-token wads

(defmethod draw-non-token-wad :around ((wad cl-syntax:expression-wad)
                                       start-ref pane cache first-line last-line)
  (typecase (cl-syntax:expression wad)
    (string (clim:with-drawing-options (pane :ink clim:+dark-goldenrod+)
              (call-next-method)))
    (t      (call-next-method))))

(defmethod draw-non-token-wad (wad start-ref pane cache first-line last-line)
  (flet ((start-line (wad) (cl-syntax:start-line wad))
         (start-column (wad) (cl-syntax:start-column wad))
         (height (wad) (cl-syntax:height wad))
         (end-column (wad) (cl-syntax:end-column wad)))
    (let ((children (cl-syntax:children wad))
          (prev-end-line start-ref)
          (prev-end-column (start-column wad))
          (ref start-ref))
      (loop for child in children
            for start-line = (start-line child)
            for height     = (height child)
            until (> (+ ref start-line) last-line)
            do (incf ref start-line)
               ;; Ensure that only at least partially visible wad are
               ;; passed to DRAW-FILTERED-AREA and DRAW-WAD.
               (when (or (<= first-line ref            last-line)      ; start visible
                         (<= first-line (+ ref height) last-line)      ; end visible
                         (<= ref first-line last-line (+ ref height))) ; contains visible region
                 (draw-filtered-area pane cache
                                     prev-end-line
                                     prev-end-column
                                     ref
                                     (start-column child)
                                     first-line last-line)
                 (draw-wad child ref pane cache first-line last-line))
               (setf prev-end-line   (+ ref height)
                     prev-end-column (end-column child)))
      (draw-filtered-area pane cache
                          prev-end-line
                          prev-end-column
                          (+ start-ref (height wad))
                          (end-column wad)
                          first-line last-line))))
