(cl:in-package #:climacs-syntax-common-lisp)

(defun parse (analyzer-stream)
  (let ((*stack* (list '())))
    (sicl-reader:read analyzer-stream)
    (first (first *stack*))))

(defun parse-and-cache (analyzer-stream)
  (push (parse analyzer-stream)
	(prefix (folio analyzer-stream))))

