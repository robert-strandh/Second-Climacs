(cl:in-package #:second-climacs-clim-view-common-lisp)

(defmethod draw-token-wad :around
    (wad (token cl-syntax:existing-symbol-token)
     start-ref pane (cache output-history) first-line last-line)
  (declare (ignorable wad token start-ref)
           (ignorable  pane cache first-line last-line))
  (if (equal (cl-syntax:package-name token)
             "COMMON-LISP")
      (clim:with-drawing-options (pane :ink clim:+purple+)
        (call-next-method))
      (call-next-method)))

(defmethod draw-token-wad :before
    (wad (token cl-syntax:non-existing-symbol-token)
     start-ref pane (cache output-history) first-line last-line)
  (declare (ignorable wad token start-ref)
           (ignorable  pane cache first-line last-line))
  (let ((pos (cl-syntax:package-marker-1 token))
        (start (cl-syntax:start-column wad))
        (end (cl-syntax:end-column wad))
        (height (cl-syntax:height wad)))
    (unless (or (null pos) (not (zerop height)))
      (draw-rectangle pane start-ref start (+ start pos 1) clim:+pink+)
      (draw-rectangle pane start-ref (+ start pos 1) end clim:+red+))))

(defmethod draw-token-wad :before
    (wad
     (token cl-syntax:non-existing-package-symbol-token)
     start-ref pane (cache output-history) first-line last-line)
  (declare (ignorable wad token start-ref)
           (ignorable  pane cache first-line last-line))
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
