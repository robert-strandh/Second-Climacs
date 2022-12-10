(cl:in-package #:second-climacs-syntax-common-lisp)

;;; MULTIPLE-VALUE-SETQ does not have a body, only a single form.  But
;;; we must do something reasonable even if the programmer got the
;;; syntax wrong, so we indent as if several forms were allowed.
(defun indent-multiple-value-setq (wad client)
  (let* ((fun #'indent-simple-list)
         (arguments (rest (children wad)))
         (indentation (+ (start-column wad) 4))
         (body-wads (compute-distinguished-indentation
                     arguments indentation fun)))
    (indent-body (+ (start-column wad) 2) body-wads client)))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:multiple-value-setq))) client)
  (indent-multiple-value-setq wad client))

