(cl:in-package #:second-climacs-base)

(define-condition climacs-error (error)
  ())

(define-condition mark-not-set (climacs-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "The mark is not set"))))

(define-condition kill-ring-is-empty (climacs-error)
  ()
  (:report "Kill ring is empty"))
