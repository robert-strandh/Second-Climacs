(cl:in-package #:second-climacs-syntax-common-lisp)

(define-condition not-in-comment (base:climacs-error)
  ()
  (:report "Not in comment"))

(defun collect-words (wads)
  (mapcar #'ip:items (alexandria:mappend #'ip:children wads)))

(defun fill-paragraph-using-wads (wads buffer cursor
                                  &rest args &key prefix
                                                  suffix
                                                  per-line-prefix)
  (declare (ignore prefix suffix per-line-prefix))
  (let* ((first        (first wads))
         (start-line   (ip:absolute-start-line-number first))
         (start-column (ip:start-column first))
         (last         (first (last wads)))
         (end-line     (ip:end-line last))
         (end-column   (ip:end-column last))
         (words        (collect-words wads)))
    (text.editing:move-cursor-to-line cursor start-line start-column)
    (text.editing:with-temporary-cursor
        (end-cursor buffer :line     end-line
                           :position end-column)
      (apply #'text.editing:fill-words cursor end-cursor words args))))

(defun fill-semicolon-comment-using-wads (wads buffer cursor)
  (let* ((first           (first wads))
         (start-column    (ip:start-column first))
         (semicolon-count (ip:semicolon-count first))
         (prefix          (format nil "~V,,,';<~>" semicolon-count))
         (per-line-prefix (format nil "~V<~>~V,,,';<~>"
                                  start-column semicolon-count)))
    (fill-paragraph-using-wads
     wads buffer cursor :prefix prefix :per-line-prefix per-line-prefix)))

(defun fill-paragraph-candidate-p (wad)
  (and (not (null wad))
       (typep wad 'ip:semicolon-comment-wad)))

;;; When this function is called, either the cursor is in a top-level
;;; semicolon comment wad so that the current wad is not NIL, or it is
;;; located before a top-level semicolon wad on the same line as the
;;; cursor, so that the current wad is nil, but the next wad is a
;;; top-level semicolon wad.

(defun fill-paragraph-top-level (current next buffer cursor)
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
                    (< (ip:absolute-start-line-number left-sibling)
                       (1- (ip:absolute-start-line-number start-wad)))
                    (/= (ip:semicolon-count left-sibling)
                        (ip:semicolon-count start-wad)))
          do (setf start-wad left-sibling))
    ;; Now collect wad descriptors of all the wads in the comment
    ;; block.
    (let ((wads
            (loop for wad = start-wad then (ip:right-sibling wad)
                  for next = (ip:right-sibling wad) then (ip:right-sibling next)
                  collect wad
                  until (or (null next)
                            (not (typep next 'ip:semicolon-comment-wad))
                            (> (ip:absolute-start-line-number next)
                               (1+ (ip:absolute-start-line-number wad)))
                            (/= (ip:semicolon-count next)
                                (ip:semicolon-count wad))))))
      (fill-semicolon-comment-using-wads wads buffer cursor))))

(defun fill-paragraph-non-top-level (start buffer cursor)
  ;; Find the first semicolon comment wad to be involved in this
  ;; operation.
  (loop for previous = (ip:left-sibling start)
        while (and (fill-paragraph-candidate-p previous)
                   (= (1- (ip:absolute-start-line-number start))
                      (ip:absolute-start-line-number previous)))
        do (setf start previous))
  ;; Collect the wads involved.
  (let ((wads (list start)))
    (loop for current = (first wads)
          for next = (ip:right-sibling current)
          while (and (fill-paragraph-candidate-p next)
                     (= (1+ (ip:absolute-start-line-number current))
                        (ip:absolute-start-line-number next)))
          do (push next wads))
    (fill-semicolon-comment-using-wads (reverse wads) buffer cursor)))

(defun fill-paragraph (cache cursor)
  (let* ((cursor-line    (cluffer:line-number cursor))
         (cursor-column  (cluffer:cursor-position cursor))
         (current        (cdr (first (ip:find-wads-containing-position
                                      cache cursor-line cursor-column))))
         (next           (unless (null current)
                           (ip:right-sibling current))))
    (when (or ;; If the cursor is inside an atomic wad, but that wad
              ;; is not a semicolon comment wad, then it is an error.
              (and (not (null current))
                   (not (typep current 'ip:comment-wad)))
              ;; The other possibility for an error is that the
              ;; current wad is NIL, and either the next wad is not a
              ;; semicolon comment wad, or it is a semicolon comment
              ;; wad, but it is on a different line from the cursor.
              (and (null current)
                   (or (not (fill-paragraph-candidate-p next))
                       (not (= cursor-line
                               (ip:absolute-start-line-number next))))))
      (error 'not-in-comment))
    (let ((buffer (cluffer:buffer cursor)))
      (cond ((and current (typep current 'ip:block-comment-wad))
             (fill-paragraph-using-wads
              (list current) buffer cursor :prefix          "#|"
                                           :per-line-prefix "  "
                                           :suffix          "|#"))
            ((null (ip:parent current))
             (fill-paragraph-top-level current next buffer cursor))
            ((null current)
             (fill-paragraph-non-top-level next buffer cursor))
            (t
             (fill-paragraph-non-top-level current buffer cursor))))))
