(cl:in-package #:second-climacs-syntax-common-lisp)

(defclass analyzer (folio-stream base:analyzer)
  ((%cache :initarg :cache :reader cache)))

(defmethod print-object ((object analyzer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A,~A"
            (current-line-number object) (current-item-number object))))

;;; Return true if and only if position A occurs strictly before
;;; position B in some buffer.
(defun position-less (line-number-a item-number-a line-number-b item-number-b)
  (or (< line-number-a line-number-b)
      (and (= line-number-a line-number-b)
           (< item-number-a item-number-b))))

;;; Return true if and only if the position indicated by the absolute
;;; line number LINE-NUMBER-1 and the column number COLUMN-NUMBER-1 is
;;; less than or equal to the position indicated by the absolute line
;;; number LINE-NUMBER-2 and the column number COLUMN-NUMBER-2.
(defun position-less-or-equal
    (line-number-1 column-number-1 line-number-2 column-number-2)
  (or (< line-number-1 line-number-2)
      (and (= line-number-1 line-number-2)
           (<= column-number-1 column-number-2))))

;;; Return true if and only if the start position of WAD is strictly
;;; less than the position indicated by LINE-NUMBER and ITEM-NUMBER.
(defun wad-starts-before-position-p (wad line-number item-number)
  (position-less (start-line wad) (start-column wad)
                 line-number item-number))

;;; Check whether there is a cached wad with a start position that
;;; corresponds to the current stream position of ANALYZER, and if so,
;;; return that wad.  If there is no such parse result, then return
;;; NIL.  If there are cached wads that entirely precede the current
;;; stream position, then remove them.
(defun cached-wad (analyzer)
  (let ((cache (cache analyzer)))
    (with-accessors ((residue residue) (suffix suffix)) cache
      (loop while (and (not (null residue))
                       (or (< (start-line (first residue))
                              (current-line-number analyzer))
                           (and (= (start-line (first residue))
                                   (current-line-number analyzer))
                                (< (start-column (first residue))
                                   (current-item-number analyzer)))))
            do (pop-from-residue cache))
      (if (not (null residue))
          (if (and (= (start-line (first residue))
                      (current-line-number analyzer))
                   (= (start-column (first residue))
                      (current-item-number analyzer)))
              (first residue)
              nil)
          (progn
            (loop while (and (not (null suffix))
                             (or (< (start-line (first suffix))
                                    (current-line-number analyzer))
                                 (and (= (start-line (first suffix))
                                         (current-line-number analyzer))
                                      (< (start-column (first suffix))
                                         (current-item-number analyzer)))))
                  do (pop-from-suffix cache))
            (if (not (null suffix))
                (if (and (= (start-line (first suffix))
                            (current-line-number analyzer))
                         (= (start-column (first suffix))
                            (current-item-number analyzer)))
                    (first suffix)
                    nil)
                nil))))))

(defun advance-stream-to-beyond-wad (analyzer wad)
  (setf (current-line-number analyzer)
        (end-line wad))
  (setf (current-item-number analyzer)
        (end-column wad)))
