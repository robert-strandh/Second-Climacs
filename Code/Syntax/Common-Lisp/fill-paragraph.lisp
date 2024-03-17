(cl:in-package #:second-climacs-syntax-common-lisp)

(define-condition not-in-paragraph-context (base:climacs-error)
  ()
  (:report "Point is not in a context that can be interpreted as a text paragraph."))

(defun collect-words (wads)
  (mapcar #'ip:items (alexandria:mappend #'ip:children wads)))

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
    (text.editing:move-cursor-to-line cursor start-line start-column)
    (text.editing:with-temporary-cursor
        (end-cursor buffer :line     end-line
                           :position end-column)
      (apply #'text.editing:fill-words cursor end-cursor words args))))

;;; Semicolon comments

(defun fill-semicolon-comment-using-wads (wads buffer cursor)
  (let* ((first           (first wads))
         (start-column    (ip:start-column first))
         (semicolon-count (ip:semicolon-count first))
         (space-count     1)
         (prefix          (format nil "~V,,,';<~>~V<~>"
                                  semicolon-count space-count))
         (per-line-prefix (format nil "~V<~>~A" start-column prefix)))
    (fill-paragraph-using-wads
     wads buffer cursor :prefix          prefix
                        :per-line-prefix per-line-prefix
                        :suffix          #.(string #\Newline))))

;;; When this function is called, either the cursor is in a top-level
;;; semicolon comment wad so that the current wad is not NIL, or it is
;;; located before a top-level semicolon wad on the same line as the
;;; cursor, so that the current wad is nil, but the next wad is a
;;; top-level semicolon wad.

(defun fill-semicolon-comment-top-level (current next buffer cursor)
  ;; Either CURRENT is not NIL, meaning the cursor is inside the wad
  ;; described by CURRENT and CURRENT is a semicolon wad, or CURRENT
  ;; is NIL meaning the cursor is located before the wad described by
  ;; NEXT on the same line as NEXT.  So either CURRENT (if CURRENT is
  ;; not NIL) or NEXT (if CURRENT is NIL) is the a wad descriptor to
  ;; start with.
  (let ((start-wad (if (null current) next current)))
    ;; Loop until start-wad is the first semicolon wad in the block.
    (loop for left-sibling = (ip:left-sibling start-wad)
          until (or (null left-sibling)
                    (not (typep left-sibling 'ip:semicolon-comment-wad))
                    (< (ip:absolute-start-line left-sibling)
                       (1- (ip:absolute-start-line start-wad)))
                    (/= (ip:semicolon-count left-sibling)
                        (ip:semicolon-count start-wad)))
          do (setf start-wad left-sibling))
    ;; Now collect all the wads in the comment block.
    (let ((wads
            (loop for wad = start-wad then (ip:right-sibling wad)
                  for next = (ip:right-sibling wad) then (ip:right-sibling next)
                  collect wad
                  until (or (null next)
                            (not (typep next 'ip:semicolon-comment-wad))
                            (> (ip:absolute-start-line next)
                               (1+ (ip:absolute-start-line wad)))
                            (/= (ip:semicolon-count next)
                                (ip:semicolon-count wad))))))
      (fill-semicolon-comment-using-wads wads buffer cursor))))

(defun fill-semicolon-comment-non-top-level (start buffer cursor)
  ;; Find the first semicolon comment wad to be involved in this
  ;; operation.
  (loop for previous = (ip:left-sibling start)
        while (and (fill-paragraph-candidate-p previous)
                   (= (1- (ip:absolute-start-line start))
                      (ip:absolute-start-line previous)))
        do (setf start previous))
  ;; Collect the wads involved.
  (let ((wads (list start)))
    (loop for current = (first wads)
          for next = (ip:right-sibling current)
          while (and (fill-paragraph-candidate-p next)
                     (= (1+ (ip:absolute-start-line current))
                        (ip:absolute-start-line next)))
          do (push next wads))
    (fill-semicolon-comment-using-wads (reverse wads) buffer cursor)))

;;; Block comments

(defun fill-block-comment (wad buffer cursor)
  (fill-paragraph-using-wads (list wad) buffer cursor :prefix          "#|"
                                                      :per-line-prefix "  "
                                                      :suffix          "|#"))

;;; String literal

(defun fill-string-literal (wad buffer cursor)
  (fill-paragraph-using-wads (list wad) buffer cursor :prefix          "\""
                                                      :per-line-prefix ""
                                                      :suffix          "\""))

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
                                     cache cursor-line cursor-column))))
         (current       (if (typep current 'ip:word-wad)
                            (ip:parent current)
                            current))
         (next          (unless (null current)
                          (ip:right-sibling current))))
    ;; TODO incrementalist should provide a function for this
    (labels ((top-level-wad (wad)
               (or (ip:parent wad) wad))
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
            ;; Different kinds of semicolon comments.
            ((null (ip:parent current))
             (fill-semicolon-comment-top-level current next buffer cursor))
            ((null current)
             (fill-semicolon-comment-non-top-level next buffer cursor))
            (t
             (fill-semicolon-comment-non-top-level current buffer cursor))))))
