(cl:in-package #:second-climacs-incremental-parsing)

(defun read-forms (analyzer)
  (with-accessors ((cache cache)
                   (current-line-number current-line-number)
                   (current-item-number current-item-number))
      analyzer
    (with-accessors ((prefix prefix) (suffix suffix) (residue residue))
        cache
      (if (null prefix)
          (setf current-line-number 0
                current-item-number 0)
          (setf current-line-number (end-line (first prefix))
                current-item-number (end-column (first prefix))))
      (let ((client (make-instance 'client :stream* analyzer)))
        (eclector.reader:call-as-top-level-read
         client
         (lambda ()
           (loop do (skip-whitespace analyzer)
                    (if (or (eof-p analyzer)
                            (and (not (null suffix))
                                 (= current-line-number
                                    (start-line (first suffix)))
                                 (= current-item-number
                                    (start-column (first suffix)))))
                        ;; If we reach EOF while reading whitespace, suffix
                        ;; must be empty, and the residue is either empty
                        ;; or it contains wads that should be removed.  If
                        ;; we do not reach EOF, then we stop only if the
                        ;; current position is that of the first parse
                        ;; result on the suffix.
                        (progn (setf residue '())
                               (return-from read-forms nil))
                        (progn
                          (parse-and-cache analyzer client)
                          (loop until (or (null residue)
                                          (> (start-line (first residue))
                                             current-line-number)
                                          (and (= (start-line (first residue))
                                                  current-line-number)
                                               (> (start-column (first residue))
                                                  current-item-number)))
                                do (pop-from-residue cache))
                          (when (null residue)
                            (loop until (or (null suffix)
                                            (> (start-line (first suffix))
                                               current-line-number)
                                            (and (= (start-line (first suffix))
                                                    current-line-number)
                                                 (> (start-column (first suffix))
                                                    current-item-number)))
                                  do (pop-from-suffix cache)))))))
         analyzer t nil nil)))))
