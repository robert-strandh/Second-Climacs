(cl:in-package #:second-climacs-syntax-common-lisp)

;;; MAPWAD calls a function for each wad in a list of wads given as
;;; argument.  A wad in the list can be relative or absolute.  If a
;;; wad is absolute, then the START-LINE of that wad is absolute.  If
;;; a wad is relative, then the START-LINE of that wad is relative to
;;; some reference.  If the first wad of the list of wads that was
;;; passed as an argument is a relative wad, then the START-LINE of
;;; that wad is relative to REFERENCE-LINE-NUMBER.  Otherwise, it is
;;; relative to the absolute start line of the previous wad in the
;;; list.

;;; FUNCTION is a function of two arguments, the wad and the absolute
;;; start line of that wad.

(defun mapwad-from-beginning (function wads reference-line-number)
  (loop for reference = reference-line-number
          then absolute-line-number
        for wad in wads
        for absolute-line-number
          = (+ (start-line wad) (if (relative-p wad) reference 0))
        do (funcall function wad absolute-line-number)))

(defun mapwad-from-end (function wads reference-line-number)
  (let ((pairs '()))
    (mapwad-from-beginning
     (lambda (wad absolute-line-number)
       (push (cons wad absolute-line-number) pairs))
     wads
     reference-line-number)
    (loop for (wad . absolute-line-number) in pairs
          do (funcall function wad absolute-line-number))))

(defun mapwad (function wads reference-line &key from-end)
  (if from-end
      (mapwad-from-end function wads reference-line)
      (mapwad-from-beginning function wads reference-line)))
