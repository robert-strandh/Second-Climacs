(cl:in-package #:second-climacs-syntax-common-lisp)

(define-condition not-in-paragraph-context (base:climacs-error)
  ()
  (:report "Point is not in a context that can be interpreted as a text paragraph."))

(defun collect-words (wads)
  (mapcar #'ip:items (alexandria:mappend #'ip:children wads)))

(defun map-text-wads (function outer-wads)
  (let ((remaining-outer outer-wads)
        (remaining-inner '()))
    (labels ((next ()
               (cond ((not (null remaining-inner))
                      (pop remaining-inner))
                     ((not (null remaining-outer))
                      (setf remaining-inner (ip:children (pop remaining-outer)))
                      (next))
                     (t
                      nil))))
      (loop :for text-wad = (next)
            :until (null text-wad)
            :do (funcall function text-wad)))))

(defun fill-paragraph-using-wads (wads buffer cursor
                                  &rest args &key prefix
                                                  suffix
                                                  per-line-prefix)
  (declare (ignore prefix suffix per-line-prefix))
  (let* ((first        (first wads))
         (start-line   (ip:absolute-start-line first))
         (start-column (ip:start-column first))
         (last         (first (last wads)))
         (end-line     (+ (ip:absolute-start-line last) (ip:height last)))
         (end-column   (ip:end-column last))
         (words        (collect-words wads)))
    (edit:move-cursor-to-line cursor start-line start-column)
    (edit:with-temporary-cursor (end-cursor buffer :line     end-line
                                                   :position end-column)
      (apply #'edit:fill-words cursor end-cursor words args))))

;;; Semicolon comments

(defun fill-semicolon-comment-using-wads (wads buffer cursor
                                          &key (indent-continuation-lines t))
  (let* ((first               (first wads))
         (start-column        (ip:start-column first))
         (semicolon-count     (ip:semicolon-count first))
         (space-count         1)
         (line-2-start-column (if indent-continuation-lines
                                  (second-line-start-column wads)
                                  nil))
         (prefix              (format nil "~V,,,';<~>~V<~>"
                                      semicolon-count space-count))
         (continuation-indent (if (null line-2-start-column)
                                  0
                                  (- line-2-start-column
                                     semicolon-count
                                     space-count)))
         (per-line-prefix     (format nil "~V<~>~A~V<~>"
                                      start-column prefix continuation-indent)))
    (fill-paragraph-using-wads
     wads buffer cursor :prefix          prefix
                        :per-line-prefix per-line-prefix
                        :suffix          #.(string #\Newline))))

;;; When this function is called, either the cursor is in a top-level
;;; semicolon comment wad so that the current wad is not NIL, or it is
;;; located before a top-level semicolon wad on the same line as the
;;; cursor, so that the current wad is nil, but the next wad is a
;;; top-level semicolon wad.

(defun compatible-semicolon-comment-p (wad semicolon-count expected-line)
  (and (not (null wad))
       (typep wad 'ip:semicolon-comment-wad)
       (= (ip:semicolon-count wad) semicolon-count)
       (= (ip:absolute-start-line wad) expected-line)))

(defun fill-semicolon-comment (start-wad buffer cursor)
  (let ((semicolon-count (ip:semicolon-count start-wad))
        (wads            (list start-wad)))
    ;; Go backwards and collect compatible wads.
    (loop :for current = start-wad :then previous
          :for previous = (ip:left-sibling current)
          :while (compatible-semicolon-comment-p
                  previous semicolon-count (1- (ip:absolute-start-line current)))
          :do (push previous wads))
    ;; Go forwards and collect compatible wads.
    (loop :for current = start-wad :then next
          :for next = (ip:right-sibling current)
          :while (compatible-semicolon-comment-p
                  next semicolon-count (1+ (ip:absolute-start-line current)))
          :collect next into following-wads
          :finally (setf wads (nconc wads following-wads)))
    (fill-semicolon-comment-using-wads wads buffer cursor)))

;;; Block comments

(defun fill-block-comment (wad buffer cursor
                           &key (indent-continuation-lines t))
  (let* ((start-column    (if indent-continuation-lines
                              (or (second-line-start-column (list wad))
                                  (+ (ip:start-column wad) 2))
                              nil))
         (per-line-prefix (if (null start-column)
                              "  "
                              (make-string
                               start-column :initial-element #\Space))))
    (fill-paragraph-using-wads (list wad) buffer cursor
                               :prefix          "#|"
                               :per-line-prefix per-line-prefix
                               :suffix          "|#")))

;;; String literal

(defun fill-string-literal (wad buffer cursor
                            &key (indent-continuation-lines t))
  (let* ((start-column    (if indent-continuation-lines
                              (or (second-line-start-column (list wad))
                                  (1+ (ip:start-column wad)))
                              nil))
         (per-line-prefix (if (null start-column)
                              nil
                              (make-string
                               start-column :initial-element #\Space))))
    (fill-paragraph-using-wads (list wad) buffer cursor
                               :prefix          "\""
                               :per-line-prefix per-line-prefix
                               :suffix          "\"")))

(defun second-line-start-column (wads)
  (let ((start-line (ip:absolute-start-line (first wads))))
    (labels ((consider-text-wad (text-wad)
               (when (> (ip:absolute-start-line text-wad) start-line)
                 (return-from second-line-start-column
                   (ip:start-column text-wad)))))
      (map-text-wads #'consider-text-wad wads))))

;;; Predicates

(defun comment-wad-p (maybe-wad)
  (and (not (null maybe-wad))
       (typep maybe-wad 'ip:comment-wad)))

(defun block-comment-wad-p (maybe-wad)
  (and (not (null maybe-wad))
       (typep maybe-wad 'ip:block-comment-wad)))

(defun string-wad-p (maybe-wad)
  (and (not (null maybe-wad))
       (typep maybe-wad 'ip:cst-wad)
       (stringp (cst:raw maybe-wad))))

(defun fill-paragraph-candidate-p (maybe-wad)
  (or (comment-wad-p maybe-wad)
      (string-wad-p maybe-wad)))

(defun fill-paragraph (cursor analyzer)
  ;; FILL-PARAGRAPH may have been performed on other sites as part of
  ;; the command that caused this call.  In that case, the buffer
  ;; content may have changed compared to the start of the command
  ;; execution and the wad graph in the cache of ANALYZER may no
  ;; longer represent the buffer content.  To ensure that the wad
  ;; graph around CURSOR is up-to-date with respect to the buffer
  ;; content, update analyzer before working with the cache.
  ;; TODO find a more general solution
  (ip:update analyzer)
  (let* ((cache         (ip:cache analyzer))
         (cursor-line   (cluffer:line-number cursor))
         (cursor-column (cluffer:cursor-position cursor))
         (current       (cdr (first (ip:find-wads-containing-position
                                     cache cursor-line cursor-column
                                     :start-relation '<= :end-relation '<=))))
         (current       (if (typep current 'ip:text-wad)
                            (ip:parent current)
                            current))
         (next          (unless (null current)
                          (ip:right-sibling current))))
    ;; TODO incrementalist should provide a function for this
    (labels ((top-level-wad (wad)
               (alexandria:if-let ((parent (ip:parent wad)))
                 (top-level-wad parent)
                 wad))
             (prepare-wad (wad)
               (unless (null wad)
                 (loop with top-level-wad = (top-level-wad wad)
                       while (or (ip::relative-p top-level-wad)
                                 (eq top-level-wad (first (ip::suffix cache))))
                       do (ip::suffix-to-prefix cache)))))
      (prepare-wad current)
      (prepare-wad next))

    (when (or ;; If the cursor is inside an atomic wad, but that wad
           ;; is neither a semicolon comment wad nor a string wad,
           ;; then it is an error.
           (and (not (null current))
                (not (or (comment-wad-p current)
                         (string-wad-p current))))
           ;; The other possibility for an error is that the
           ;; current wad is NIL, and either the next wad is not a
           ;; semicolon comment wad, or it is a semicolon comment
           ;; wad, but it is on a different line from the cursor.
           (and (null current)
                (or (not (fill-paragraph-candidate-p next))
                    (not (= cursor-line
                            (ip:absolute-start-line next))))))
      (error 'not-in-paragraph-context))
    (let ((buffer (cluffer:buffer cursor)))
      (cond ((block-comment-wad-p current)
             (fill-block-comment current buffer cursor))
            ((string-wad-p current)
             (fill-string-literal current buffer cursor))
            ;; Semicolon comments.
            ((null current)
             (fill-semicolon-comment next buffer cursor))
            (t
             (fill-semicolon-comment current buffer cursor))))))
