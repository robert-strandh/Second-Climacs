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

;;; Return true if and only if the position indicated by
;;; RELATIVE-LINE-NUMBER and COLUMN-NUMBER is entirely before WAD.  If
;;; WAD is an absolute wad, then RELATIVE-LINE-NUMBER must be the
;;; absolute line number of the position.  If WAD is a relative wad,
;;; then RELATIVE-LINE-NUMBER must be the difference between the
;;; absolute line number of the position, and the start line of the
;;; wad to which WAD is relative.  The position is before WAD if
;;; either RELATIVE-LINE-NUMBER is strictly less than the start line
;;; of WAD, or if RELATIVE-LINE-NUMBER is equal to the start line of
;;; WAD, and COLUMN-NUMBER is less than or equal to the start column
;;; of WAD.
(defun position-is-before-wad-p (wad relative-line-number column-number)
  (or (< relative-line-number (start-line wad))
      (and (= relative-line-number (start-line wad))
           (<= column-number (start-column wad)))))

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

;;; We do a quick check to see that the prefix is not empty, and that
;;; the line number of the position we are looking for is at most the
;;; end line of the first wad of the prefix, i.e., the prefix wad that
;;; is the furthest away from the beginning of the buffer.
(defun wad-might-be-inside-prefix-p (cache line-number)
  (let ((prefix (prefix cache)))
    (and (not (null prefix))
         (let* ((wad (first prefix))
                (start-line (start-line wad)))
           (<= line-number (+ start-line (height wad)))))))

;;; We do a quick check to see that the suffix is not empty, and that
;;; the line number of the position we are looking for is at least the
;;; start line of the first wad of the suffix, i.e., the suffix wad
;;; that is the closest to the beginning of the buffer.  Recall that
;;; the first wad on the suffix is an absolute wad.
(defun wad-might-be-inside-suffix-p (cache line-number)
  (let ((suffix (suffix cache)))
    (and (not (null suffix))
         (>= line-number (start-line (first suffix))))))
