(cl:in-package #:climacs-syntax-common-lisp)

(defun read-forms (analyzer)
  (with-accessors ((cache folio)
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
      (loop do (skip-whitespace analyzer)
               (if (or (eof-p analyzer)
                       (and (not (null suffix))
                            (= current-line-number
                               (start-line (first suffix)))
                            (= current-item-number
                               (start-column (first suffix)))))
                   ;; If we reach EOF while reading whitespace, then the
                   ;; residue and the suffix must be empty.  If we do not
                   ;; reach EOF, then we stop only if the current position
                   ;; is that of the first parse result on the suffix.
                   (return-from read-forms nil)
                   (let ((next (parse analyzer)))
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
                             do (pop-from-suffix cache)))
                     (push-to-prefix cache next)))))))
