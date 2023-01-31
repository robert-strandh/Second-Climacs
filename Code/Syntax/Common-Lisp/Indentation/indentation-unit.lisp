(cl:in-package #:second-climacs-syntax-common-lisp)

;;; An indentation unit is a list of wads that are consecutive in the
;;; buffer, such that only the first wad in the list starts a line.
;;; Every other wad W in the list is preceded by a wad V such that the
;;; last line of V is the first line of W.  If the first wad in the
;;; list is an EXPRESSION-WAD W, then the indentation of W is
;;; determined by its parent wad.  If the first wad in the list is a
;;; NO-EXPRESSION-WAD W, but a subsequent indentation unit starts with
;;; an EXPRESSION-WAD V, then the indentation of W is the same as the
;;; indentation of V.  If the first wad in the list is a
;;; NO-EXPRESSION-WAD W, but there is no subsequent indentation unit
;;; starts with an EXPRESSION-WAD, then the indentation of W is the
;;; same as if it were an EXPRESSION-WAD, so determined by the parent
;;; wad.  The indentation of a wad W, other than the fist,in an
;;; indentation unit, is computed independently, so determined only by
;;; the start column of W.

;;; This function is required because semicolon comment wads actually
;;; have a height of 1.
(defun effective-height (wad)
  (if (typep wad 'semicolon-comment-wad)
      0
      (height wad)))

(defun compute-indentation-units (wads)
  (if (null wads)
      '()
      (let* ((result '())
             (first (first wads))
             (current-indentation-unit (list first))
             (seen-expression-wad-p (typep first 'expression-wad)))
          (loop for wad in (rest wads)
                do (if (or (= (start-line wad)
                              (effective-height (first current-indentation-unit)))
                           (not seen-expression-wad-p))
                       (push wad current-indentation-unit)
                       ;; Else we add the current indentation unit to
                       ;; the result, and start a new indentation
                       ;; unit.
                       (progn
                         (push (reverse current-indentation-unit) result)
                         (setf seen-expression-wad-p nil)
                         (setf current-indentation-unit (list wad))))
                   (when (typep wad 'expression-wad)
                     (setf seen-expression-wad-p t)))
        (push (reverse current-indentation-unit) result)
        (reverse result))))
