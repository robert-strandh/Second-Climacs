(cl:in-package #:second-climacs-clim-view-common-lisp)

(defmethod draw-token-wad :around
    (wad (token climacs-syntax-common-lisp:existing-symbol-token)
     start-ref pane (cache output-history) first-line last-line)
  (declare (ignorable wad token start-ref)
           (ignorable  pane cache first-line last-line))
  (if (equal (climacs-syntax-common-lisp:package-name token)
             "COMMON-LISP")
      (clim:with-drawing-options (pane :ink clim:+purple+)
        (call-next-method))
      (call-next-method)))

(defmethod draw-token-wad :before
    (wad (token climacs-syntax-common-lisp:non-existing-symbol-token)
     start-ref pane (cache output-history) first-line last-line)
  (declare (ignorable wad token start-ref)
           (ignorable  pane cache first-line last-line))
  (let ((pos (climacs-syntax-common-lisp:package-marker-1 token))
        (start (climacs-syntax-common-lisp:start-column wad))
        (end (climacs-syntax-common-lisp:end-column wad))
        (height (climacs-syntax-common-lisp:height wad)))
    (unless (or (null pos) (not (zerop height)))
      (draw-rectangle pane start-ref start (+ start pos 1) clim:+pink+)
      (draw-rectangle pane start-ref (+ start pos 1) end clim:+red+))))

(defmethod draw-token-wad :before
    (wad
     (token climacs-syntax-common-lisp:non-existing-package-symbol-token)
     start-ref pane (cache output-history) first-line last-line)
  (declare (ignorable wad token start-ref)
           (ignorable  pane cache first-line last-line))
  (let ((pos (climacs-syntax-common-lisp:package-marker-1 token))
        (start (climacs-syntax-common-lisp:start-column wad))
        (end (climacs-syntax-common-lisp:end-column wad))
        (height (climacs-syntax-common-lisp:height wad)))
    (unless (or (null pos) (not (zerop height)))
      (draw-rectangle pane start-ref start (+ start pos) clim:+red+)
      (draw-rectangle pane start-ref (+ start pos) end clim:+pink+))))

(defmethod draw-token-wad
    (wad token start-ref pane (cache output-history) first-line last-line)
  (let* ((start-column (climacs-syntax-common-lisp:start-column wad))
         (end-column (climacs-syntax-common-lisp:end-column wad))
         (height (climacs-syntax-common-lisp:height wad)))
    (draw-filtered-area pane cache
                        start-ref
                        start-column
                        (+ start-ref height)
                        end-column
                        first-line last-line)))
