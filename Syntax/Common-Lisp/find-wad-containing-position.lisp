(cl:in-package #:climacs-syntax-common-lisp)

;;; Given a position in the form of a line number and a column number,
;;; return the innermost wad that contains that position, or NIL of no
;;; wad contains the position.  For some wad W, if the line number of
;;; the position is strictly greater than the absolute start line of W
;;; and strictly less than the end line of W, then W contains the
;;; position.  If the line number of the position is the same as the
;;; start line of W, then the column number must be strictly greater
;;; than the start column of W (otherwise, the cursor is positioned
;;; before the wad).  If the line number of the position is the same
;;; as the end line of W, then the column number must be strictly less
;;; than the end column of W (otherwise, the cursor is positioned
;;; after the wad).  Notice that the line number of the position may
;;; be both the start line and the end line of the wad, in which case
;;; both the last criteria mentioned must be fulfilled.

(defun position-is-inside-wad-p
    (wad line-number column-number previous-line-number)
  (let ((relative-line-number
          (- line-number (if (relative-p wad) previous-line-number 0))))
    (not (or (< relative-line-number (start-line wad))
             (> relative-line-number (+ (start-line wad) (height wad)))
             (and (= relative-line-number (start-line wad))
                  (<= column-number (start-column wad)))
             (and (= relative-line-number (+ (start-line wad) (height wad)))
                  (>= column-number (end-column wad)))))))
