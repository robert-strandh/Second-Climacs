(cl:in-package #:second-climacs-syntax-common-lisp)

;;; An indentation element is a list of wads that are consecutive in
;;; the buffer, such that only the first wad in the list starts a
;;; line.  Every other wad W in the list is preceded by a wad V such
;;; that the last line of V is the first line of W.  If the first wad
;;; in the list is an EXPRESSION-WAD W, then the indentation of W is
;;; determined by its parent wad.  If the first wad in the list is a
;;; NO-EXPRESSION-WAD W, but a subsequent indentation element starts
;;; with an EXPRESSION-WAD V, then the indentation of W is the same as
;;; the indentation of V.  If the first wad in the list is a
;;; NO-EXPRESSION-WAD W, but there is no subsequent indentation
;;; element starts with an EXPRESSION-WAD, then the indentation of W
;;; is the same as if it were an EXPRESSION-WAD, so determined by the
;;; parent wad.  The indentation of a wad W, other than the fist,in an
;;; indentation element, is computed independently, so determined only
;;; by the start column of W.

;;; An indentation unit is a list of indentation elements such that
;;; only the last indentation element can have an EXPRESSION-WAD as
;;; its first wad.

;;; This function takes a list of wads and returns a list of
;;; indentation units.
(defun compute-indentation-units (wads)
  (if (null wads)
      '()
      (let* ((result '())
             (current-indentation-unit '())
             (current-indentation-element (list (first wads))))
          (loop for wad in (rest wads)
                do (if (= (start-line wad)
                          (height (first current-indentation-element)))
                       ;; Then WAD does not start a line, so add it to
                       ;; the current indentation element.
                       (push wad current-indentation-element)
                       (progn
                         ;; WAD starts a new line, so add the current
                         ;; indentation element to the current
                         ;; indentation unit, and start a new
                         ;; indentation element.
                         (push (reverse current-indentation-element)
                               current-indentation-unit)
                         (setf current-indentation-element (list wad))
                         (when (and (typep wad 'expression-wad)
                                    (typep (first (first current-indentation-unit))
                                           'expression-wad))
                           ;; Then we add the current indentation unit
                           ;; to the result, and start a new
                           ;; indentation unit.
                           (push (reverse current-indentation-unit) result)
                           (setf current-indentation-unit '())))))
        (push (reverse current-indentation-element)
              current-indentation-unit)
        (push (reverse current-indentation-unit) result)
        (reverse result))))
