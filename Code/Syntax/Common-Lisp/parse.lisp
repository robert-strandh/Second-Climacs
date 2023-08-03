(cl:in-package #:second-climacs-syntax-common-lisp)

;;; PARSE-AND-CACHE collects Eclector errors in *ERRORS* during
;;; parsing and ADD-CHILDREN adds the resulting ERROR-WADs to the
;;; appropriate nodes in the wad tree.
(defvar *errors*)

;;; Establish a handler which collects READER-ERRORs into *ERRORS* and
;;; recovers around BODY.
(defmacro with-error-recording (() &body body)
  `(handler-bind
       ((reader-error
          (lambda (condition)
            (destructuring-bind (line . column)
                (eclector.base:stream-position condition)
              (let ((line-width (line-length (folio analyzer)
                                             (current-line-number analyzer)))
                    (column     (max 0 (+ column (eclector.base:position-offset condition)))))
                (push (make-wad 'error-wad :max-line-width line-width
                                           :children       '()
                                           :start-line     line
                                           :start-column   column
                                           :height         0
                                           :end-column     column
                                           :relative-p     nil
                                           :condition      condition)
                      *errors*)))
            (eclector.reader:recover))))
     ,@body))

;;; Add the wads in EXTRA-CHILDREN to WAD.
(defun add-children (wad extra-children)
  ;; Make descendants of WAD absolute so we can compute the
  ;; appropriate insertion points for EXTRA-CHILDREN.
  (labels ((rec (wad)
             (let ((children (children wad)))
               (when children
                 (when (every #'relative-p children)
                   (make-absolute children (start-line wad)))
                 (map nil #'rec children)))))
    (rec wad))

  (let* (;; Merge children of WAD and EXTRA-CHILDREN based on their
         ;; positions.
         (children     (merge 'list (copy-list (children wad)) (copy-list extra-children)
                              (lambda (left right)
                                (or (< (start-line left) (start-line right)) ; TODO make a convenience function for this
                                    (and (= (start-line left) (start-line right))
                                         (< (start-column left) (start-column right)))))))
         ;; Possibly push some elements of EXTRA-CHILDREN into
         ;; existing children of WAD. A wad E in EXTRA-CHILDREN must
         ;; become a child of an existing child C of WAD if the source
         ;; range of E is contained in the source range of C.
         (children*    (loop for last-child = nil then child
                             for last-end-line = nil then (end-line last-child)
                             for last-end-column = nil then (end-column last-child) ; TODO consider lines
                             for child in children
                             if (and (member child extra-children)
                                     (not (null last-end-column))
                                     (or (< (end-line child) last-end-line)
                                         (and (= (end-line child) last-end-line)
                                              (< (start-column child) last-end-column)
                                              (<= (end-column child) last-end-column))))
                               do (add-children last-child (list child))
                             else if (cond ((not (member child extra-children))
                                            t)
                                           ((and (or (< (end-line child) (end-line wad))
                                                     (and (= (end-line child) (end-line wad))
                                                          (<= (end-column child) (end-column wad))))
                                                 (not (relative-p wad)))
                                            (assert (not (relative-p wad)))
                                            t)
                                           (t
                                            (warn "Dropping ~A (~A)~&  parent         ~A~&  previous child ~A"
                                                  child (class-name (class-of (condition* child)))
                                                  wad
                                                  last-child)
                                            nil))
                               collect child)))
    (reinitialize-instance wad :children children*)))

(defmethod reader:read-maybe-nothing :around
    ((client client) (stream analyzer) eof-error-p eof-value)
  (let ((cached (cached-wad stream)))
    (if (null cached)
        ;; Nothing has been cached, so call
        ;; READ-MAYBE-NOTHING. Collect errors in *ERRORS* and integrate
        ;; them into the wad tree.
        (let ((*errors* '()))
          (multiple-value-bind (object kind result)
              (call-next-method)
            (when *errors*
              (add-children result (reverse *errors*)))
            (values object kind result)))
        ;; There is a cached wad for the current input position. Turn
        ;; the wad into appropriate return values, inject it into
        ;; Eclector's result stack and advance STREAM.
        (multiple-value-prog1
            (etypecase cached
              (skipped-wad    (values nil                 :skip   cached t))
              (expression-wad (values (expression cached) :object cached t))
              (error-wad      (values nil                 :skip   cached t)))
          (assert (not (relative-p cached)))
          (push cached (first eclector.parse-result::*stack*)) ; HACK
          (advance-stream-to-beyond-wad stream cached)))))

(defun parse-and-cache (analyzer client)
  ;; Use ECLECTOR.READER:READ-MAYBE-NOTHING to read either a single
  ;; skipped input or an object. Both are pushed into the prefix of
  ;; the FOLIO-STREAM. For any error during parsing,
  ;; WITH-ERROR-RECORDING creates an ERROR-WAD records it in *ERRORS*,
  ;; then asks Eclector to perform the appropriate recovery. The
  ;; READ-MAYBE-NOTHING method takes care of integrating the collected
  ;; ERROR-WADs into the wad tree.
  (multiple-value-bind (object kind wad)
      (with-error-recording ()
        (eclector.reader:read-maybe-nothing client analyzer nil nil))
    (declare (ignore object))
    (case kind
      (:eof)              ; nothing to do for end of input
      (:whitespace)       ; nothing to do for whitespace
      (t                  ; got a tree of absolute wads. make relative
       (labels ((rec (wad)
                  (unless (relative-p wad) ; TODO can it be relative due to caching or what?
                    (let ((children (children wad)))
                      (when children
                        (map nil #'rec children)
                        (unless (every #'relative-p children)
                          (make-relative children (start-line wad))))))))
         (rec wad))
       (compute-form-indentation wad nil client)
       (push-to-prefix (cache analyzer) wad)))))
