(cl:in-package #:second-climacs-clim-view-common-lisp)

(defmethod draw-token-parse-result :around
    (parse-result (token climacs-syntax-common-lisp:existing-symbol-token)
     start-ref pane (cache output-history) first-line last-line)
  (declare (ignorable parse-result token start-ref)
           (ignorable  pane cache first-line last-line))
  (if (equal (climacs-syntax-common-lisp:package-name token)
             "COMMON-LISP")
      (clim:with-drawing-options (pane :ink clim:+purple+)
        (call-next-method))
      (call-next-method)))

(defmethod draw-token-parse-result :before
    (parse-result (token climacs-syntax-common-lisp:non-existing-symbol-token)
     start-ref pane (cache output-history) first-line last-line)
  (declare (ignorable parse-result token start-ref)
           (ignorable  pane cache first-line last-line))
  (let ((pos (climacs-syntax-common-lisp:package-marker-1 token))
        (start (climacs-syntax-common-lisp:start-column parse-result))
        (end (climacs-syntax-common-lisp:end-column parse-result))
        (height (climacs-syntax-common-lisp:height parse-result)))
    (unless (or (null pos) (not (zerop height)))
      (draw-rectangle pane start-ref start (+ start pos 1) clim:+pink+)
      (draw-rectangle pane start-ref (+ start pos 1) end clim:+red+))))

(defmethod draw-token-parse-result :before
    (parse-result
     (token climacs-syntax-common-lisp:non-existing-package-symbol-token)
     start-ref pane (cache output-history) first-line last-line)
  (declare (ignorable parse-result token start-ref)
           (ignorable  pane cache first-line last-line))
  (let ((pos (climacs-syntax-common-lisp:package-marker-1 token))
        (start (climacs-syntax-common-lisp:start-column parse-result))
        (end (climacs-syntax-common-lisp:end-column parse-result))
        (height (climacs-syntax-common-lisp:height parse-result)))
    (unless (or (null pos) (not (zerop height)))
      (draw-rectangle pane start-ref start (+ start pos) clim:+red+)
      (draw-rectangle pane start-ref (+ start pos) end clim:+pink+))))

(defmethod draw-token-parse-result :before
    (parse-result (token climacs-syntax-common-lisp:illegal-symbol-token)
     start-ref pane (cache output-history) first-line last-line)
  (declare (ignorable parse-result token start-ref)
           (ignorable  pane cache first-line last-line))
  (let ((pos1 (climacs-syntax-common-lisp:package-marker-1 token))
        (pos2 (climacs-syntax-common-lisp:package-marker-2 token))
        (start (climacs-syntax-common-lisp:start-column parse-result))
        (end (climacs-syntax-common-lisp:end-column parse-result))
        (height (climacs-syntax-common-lisp:height parse-result)))
    (unless (plusp height)
      (cond ((and (null pos1) (null pos2))
             (draw-rectangle pane start-ref start end clim:+red+))
            ((null pos1)
             (draw-rectangle pane start-ref start (+ start pos2) clim:+pink+)
             (draw-rectangle pane start-ref (+ start pos2) (+ start pos2 1) clim:+red+)
             (draw-rectangle pane start-ref (+ start pos2 1) end clim:+pink+))
            ((null pos2)
             (draw-rectangle pane start-ref start (+ start pos1) clim:+pink+)
             (draw-rectangle pane start-ref (+ start pos1) (+ start pos1 1) clim:+red+)
             (draw-rectangle pane start-ref (+ start pos1 1) end clim:+pink+))
            (t
             (draw-rectangle pane start-ref start (+ start pos1) clim:+pink+)
             (draw-rectangle pane start-ref (+ start pos1) (+ start pos1 1) clim:+red+)
             (draw-rectangle pane start-ref (+ start pos1 1) (+ start pos2) clim:+pink+)
             (draw-rectangle pane start-ref (+ start pos2) (+ start pos2 1) clim:+red+)
             (draw-rectangle pane start-ref (+ start pos2 1) end clim:+pink+))))))

(defmethod draw-token-parse-result
    (parse-result token
     start-ref pane (cache output-history) first-line last-line)
  (let* ((pr parse-result)
         (start-column (climacs-syntax-common-lisp:start-column pr))
         (end-column (climacs-syntax-common-lisp:end-column pr))
         (height (climacs-syntax-common-lisp:height pr)))
    (draw-filtered-area pane cache
                        start-ref
                        start-column
                        (+ start-ref height)
                        end-column
                        first-line last-line)))
