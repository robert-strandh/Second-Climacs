(cl:in-package #:second-climacs-syntax-common-lisp)

(define-condition not-in-comment (base:climacs-error)
  ()
  (:report "Not in comment"))

(defun collect-words (wad-descriptors buffer)
  (loop with words = '()
        with word = (make-array 0 :fill-pointer t
                                  :adjustable t
                                  :element-type 'character)
        for wad-descriptor in wad-descriptors
        for wad = (wad wad-descriptor)
        for semicolon-count = (semicolon-count wad)
        for start-column = (start-column-number wad-descriptor)
        for line-number = (start-line-number wad-descriptor)
        for line = (cluffer:find-line buffer line-number)
        for length = (cluffer:item-count line)
        do (loop for index from (+ start-column semicolon-count)
                   below length
                 for item = (cluffer:item-at-position line index)
                 do (if (eql item #\Space)
                        (unless (zerop (fill-pointer word))
                          (push (copy-seq word) words)
                          (setf (fill-pointer word) 0))
                        (vector-push-extend item word))
                 finally (push (copy-seq word) words)
                         (setf (fill-pointer word) 0))
           finally (return (nreverse words))))

(defparameter *fill-column* 72)

(defun fill-paragraph-using-wad-descriptors (wad-descriptors buffer cursor)
  (let* ((first (first wad-descriptors))
         (start-line-number (start-line-number first))
         (start-column-number (start-column-number first))
         (semicolon-count (semicolon-count (wad first)))
         (last (first (last wad-descriptors)))
         (end-cursor (make-instance 'base:standard-cursor :buffer buffer))
         (words (collect-words wad-descriptors buffer)))
    (base:set-cursor-positions cursor start-line-number start-column-number)
    (cluffer:attach-cursor
     end-cursor
     (cluffer:find-line buffer (end-line-number last))
     (end-column-number last))
    (loop until (base:cursor-= cursor end-cursor)
          do (base:delete-item cursor))
    (cluffer:detach-cursor end-cursor)
    (flet ((insert-word (word)
             (base:insert-item cursor #\Space)
             (loop for char across word
                   do (base:insert-item cursor char)))
           (indent ()
             (loop repeat start-column-number
                   do (base:insert-item cursor #\Space)))
           (insert-semicolons ()
             (loop repeat semicolon-count
                   do (base:insert-item cursor #\;))))
      (insert-semicolons)
      (insert-word (first words))
      (loop for word in (rest words)
            do (when (>= (+ (cluffer:cursor-position cursor) (length word))
                         *fill-column*)
                 (base:insert-item cursor #\Newline)
                 (indent)
                 (insert-semicolons))
               (insert-word word))
      (base:insert-item cursor #\Newline)
      (base:set-cursor-positions
       cursor
       (start-line-number first)
       (start-column-number first)))))

(defun fill-paragraph-candidate-p (wad-descriptor)
  (and (not (null wad-descriptor))
       (typep (wad wad-descriptor) 'semicolon-comment-wad)))

(defun fill-paragraph-top-level (cache wad-descriptor buffer cursor)
  (unless (null wad-descriptor)
    (push-to-suffix cache (pop-from-prefix cache)))
  ;; Now, the wad indicated by the cursor is always the first wad on
  ;; the suffix.
  (with-accessors ((prefix prefix) (suffix suffix)) cache
    (loop until (or (null prefix)
                    (not (typep (first prefix) 'semicolon-comment-wad))
                    (< (start-line (first prefix))
                       (1- (start-line (first suffix))))
                    (/= (semicolon-count (first prefix))
                        (semicolon-count (first suffix))))
          do (push-to-suffix cache (pop-from-prefix cache)))
    ;; Now, all the wads we are interested in are on the suffix.
    (let ((wad-descriptors
            (list (make-wad-descriptor-from-wad (first suffix)))))
      (push-to-prefix cache (pop-from-suffix cache))
      (loop until (or (null suffix)
                      (not (typep (first suffix) 'semicolon-comment-wad))
                      (> (start-line (first suffix))
                         (1+ (start-line (first prefix))))
                      (/= (semicolon-count (first prefix))
                          (semicolon-count (first suffix))))
            do (push (make-wad-descriptor-from-wad (first suffix))
                     wad-descriptors)
               (push-to-prefix cache (pop-from-suffix cache)))
      (fill-paragraph-using-wad-descriptors
       (reverse wad-descriptors) buffer cursor))))

(defun fill-paragraph-non-top-level (wad-descriptor buffer cursor)
  (let ((start wad-descriptor))
    ;; Find the first semicolon comment wad to be involved in this
    ;; operation.
    (loop for previous = (previous-sibling start)
          while (and (fill-paragraph-candidate-p previous)
                     (= (1- (start-line-number start))
                        (start-line-number previous)))
          do (setf start previous))
    ;; Collect the wad descriptors involved.
    (let ((wad-descriptors (list start)))
      (loop for current = (first wad-descriptors)
            for next = (next-sibling current)
            while (and (fill-paragraph-candidate-p next)
                       (= (1+ (start-line-number current))
                          (start-line-number next)))
            do (push next wad-descriptors))
      (fill-paragraph-using-wad-descriptors
       (reverse wad-descriptors) buffer cursor))))

(defun fill-paragraph (cache cursor)
  (multiple-value-bind (current parent previous next)
      (compute-wad-descriptors cache cursor)
    (declare (ignore previous))
    (let ((cursor-line-number (cluffer:line-number cursor)))
      (when (or
             ;; If the cursor is inside an atomic wad, but that wad is
             ;; not a semicolon comment wad, then it is an error.
             (and (not (null current))
                  (not (typep (wad current) 'semicolon-comment-wad)))
             ;; The other possibility for an error is that the current
             ;; wad is NIL, and either the next wad is not a semicolon
             ;; comment wad, or it is a semicolon comment wad, but it is
             ;; on a different line from the cursor.
             (and (null current)
                  (or (not (fill-paragraph-candidate-p next))
                      (not (= cursor-line-number
                              (start-line-number next))))))
        (error 'not-in-comment)))
    (let ((buffer (cluffer:buffer cursor)))
      (if (null parent)
          (fill-paragraph-top-level cache current buffer cursor)
          (if (null current)
              (fill-paragraph-non-top-level next buffer cursor)
              (fill-paragraph-non-top-level current buffer cursor))))))
