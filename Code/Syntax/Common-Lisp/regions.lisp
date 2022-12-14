(cl:in-package #:second-climacs-syntax-common-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; MARK-EXPRESSION.

(defun mark-expression (cache cursor)
  (base:set-the-mark cursor)
  (forward-expression cache cursor)
  (base:exchange-cursor-and-mark cursor))
