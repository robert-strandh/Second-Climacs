(cl:in-package #:second-climacs-clim-base)

;;; Return the text-style width and the text-style height of PANE as
;;; two values.
;; TODO copied from Common Lisp view
(defun text-style-dimensions (pane)
  (let ((text-style (clim:medium-text-style pane)))
    (values (clim:text-style-width text-style pane)
            (clim:text-style-height text-style pane))))

;;; Return the area of the viewport of PANE in units of line and
;;; column number.  We return only integers, so that if a fraction of
;;; a line or a column is included in the viewport, then the entire
;;; line or column is included in the return values.  Four values are
;;; returned: The column and the line of the upper-left corner and the
;;; column and the line of the lower-right corner.
;; TODO copied from Common Lisp view
(defun viewport-area (pane)
  (let ((region (clim:pane-viewport-region pane)))
    (clim:with-bounding-rectangle* (left top right bottom) region
      (multiple-value-bind (width height) (text-style-dimensions pane)
        (values (floor left width) (floor top height)
                (ceiling right width) (ceiling bottom height))))))

(defun scroll-to (pane line column)
  (multiple-value-bind (width height) (text-style-dimensions pane)
    (clim:scroll-extent
     (clim:sheet-parent pane) (* column width) (* line height))))

(defun scroll (pane line-amount column-amount)
  (multiple-value-bind (column line) (viewport-area pane)
    (scroll-to pane (+ line line-amount) (+ column column-amount))))

(defun move-viewport-to-cursor (pane)
  (let* ((clim-view (clim:stream-default-view pane))
         (climacs-view (climacs-view clim-view))
         (site (base:site climacs-view))
         (cursor (edit:point site))
         (cursor-line-number (cluffer:line-number cursor))
         (cursor-column-number (cluffer:cursor-position cursor)))
    (multiple-value-bind (left top right bottom)
        (viewport-area pane)
      (unless (and (<= (1+ top) cursor-line-number (- bottom 2))
                   (<= left cursor-column-number right))
        (scroll-to pane
                   (max 0 (- cursor-line-number
                             (floor (- bottom top) 2)))
                   (max 0 (- cursor-column-number
                             (floor (- right left) 2))))))))

(defun move-cursor-to-viewport (pane)
  (let* ((clim-view (clim:stream-default-view pane))
         (climacs-view (climacs-view clim-view))
         (site (base:site climacs-view))
         (cursor (edit:point site))
         (cursor-line-number (cluffer:line-number cursor))
         (cursor-column-number (cluffer:cursor-position cursor))
         (buffer (cluffer:buffer cursor))
         (line-count (cluffer:line-count buffer)))
    (multiple-value-bind (left top right bottom)
        (viewport-area pane)
      (unless (and (<= top cursor-line-number bottom)
                   (<= left cursor-column-number right))
        (let ((new-top (min top (1- line-count)))
              (new-bottom (max top bottom)))
          (cond ((> cursor-line-number new-bottom)
                 (let* ((buffer (cluffer:buffer cursor))
                        (line (cluffer:find-line buffer new-bottom)))
                   (cluffer:detach-cursor cursor)
                   (cluffer:attach-cursor cursor line)))
                ((< cursor-line-number new-top)
                 (let* ((buffer (cluffer:buffer cursor))
                        (line (cluffer:find-line buffer new-top)))
                   (cluffer:detach-cursor cursor)
                   (cluffer:attach-cursor cursor line)))
                ((< cursor-column-number left)
                 (setf (cluffer:cursor-position cursor) left))
                ((> cursor-column-number right)
                 (setf (cluffer:cursor-position cursor) right))
                (t
                 nil)))))))
