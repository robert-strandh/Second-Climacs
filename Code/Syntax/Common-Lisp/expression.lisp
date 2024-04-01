(cl:in-package #:second-climacs-syntax-common-lisp)

;;; Expression interface

(defmethod edit.exp:range ((expression ip:wad))
  (let* ((start-line   (ip:absolute-start-line expression))
         (start-column (ip:start-column expression))
         (end-line     (+ start-line (ip:height expression)))
         (end-column   (ip:end-column expression)))
    (values start-line start-column end-line end-column)))

(defmethod edit.exp:children ((expression ip:wad))
  (ip:children expression))

(defmethod edit.exp:children ((expression ip:atom-wad))
  ;; Present a pathname as an atom with potential `text-wad' children,
  ;; but skip the `atom-wad' for the string in between.
  (if (typep (cst:raw expression) 'pathname)
      (alexandria:mappend #'ip:children (ip:children expression))
      (ip:children expression)))

(defmethod edit.exp:map-expressions-containing-cursor-using-buffer
    ((function    t)
     (buffer      base:standard-buffer)
     (cursor      cluffer:cursor)
     (syntax-tree t)
     &key (start-relation '<=) (end-relation '<))
  (let* ((view          (uiop:symbol-call '#:second-climacs-clim-base '#:current-view)) ; TODO
         (analyzer      (base:analyzer view))
         (cache         (ip:cache analyzer))
         (cursor-line   (cluffer:line-number cursor))
         (cursor-column (cluffer:cursor-position cursor))
         ;; Defer reporting some wads so that we can filter based on
         ;; the parent. The use case is not reporting the string
         ;; `atom-wad' child of a pathname `atom-wad'.
         (deferred      nil)
         (innermostp    t))
    (flet ((flush ()
             (when (not (null deferred))
               (funcall function deferred)
               (setf deferred nil))))
      (ip:map-wads-containing-position
       (lambda (absolute-line-number wad)
         (declare (ignore absolute-line-number))
         (cond ((not innermostp)
                (when (not (null deferred))
                  (if (and (typep wad 'ip:atom-wad)
                           (typep (cst:raw wad) 'pathname))
                      (setf deferred nil)
                      (flush)))
                (funcall function wad))
               ((typep wad 'ip:text-wad)) ; filter out `text-wad's
               (t
                (setf deferred wad)
                (setf innermostp nil))))
       cursor-line cursor-column cache :start-relation start-relation
       :end-relation   end-relation)
      (flush))))
