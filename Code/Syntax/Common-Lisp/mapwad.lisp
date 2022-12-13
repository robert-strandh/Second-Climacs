(cl:in-package #:second-climacs-syntax-common-lisp)

;;; There are three possible lists of wads that can be given to us.
;;; It can be a list of absolute wads, coming from the cache prefix.
;;; In that case, we want to ignore REFERENCE-LINE altogether and just
;;; use the absolute line numbers of the wads in the list.  It can be
;;; a list where the first wad is absolute and the remaining wads are
;;; relative, coming form the cache suffix.  In that case, we again
;;; want to ignore REFERENCE-LINE, and use the absolute line of the
;;; first wad as a reference for the remaining wads.  Or it can be a
;;; list of relative wads.  In that case, REFERENCE-LINE is the
;;; absolute line number to which the first wad is relative.

;;; FUNCTION is a function of two arguments, the wad and the absolute
;;; start line of that wad.

(defun mapwad-from-beginning (function wads reference-line-number)
  (loop for reference = reference-line-number
          then absolute-line-number
        for wad in wads
        for absolute-line-number
          = (+ (start-line wad) (if (relative-p wad) reference 0))
        do (funcall function wad absolute-line-number)))

(defun mapwad (function wads reference-line &key from-end)
  (if from-end
      (mapwad-from-end function wads reference-line)
      (mapwad-from-beginning function wads reference-line)))
