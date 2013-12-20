(cl:in-package #:climacs-syntax-common-lisp)

(defun syntax-type (char)
  (multiple-value-bind (function non-terminating-p)
      (get-macro-character char)
    (cond ((not (null function))
	   (if non-terminating-p
	       :non-terminating-macro
	       :terminating-macro))
	  ((member char '(#\Space #\Tab #\Linefeed #\Return #\Page))
	   :whitespace)
	  ((eql char #\\)
	   :single-escape)
	  ((eql char #\|)
	   :multiple-escape)
	  (t
	   :constituent))))

(defun init-readtable (readtable)
  (set-macro-character #\( 'left-parenthesis nil readtable)
  (set-macro-character #\) 'right-parenthesis nil readtable)
  (set-macro-character #\' 'single-quote nil readtable)
  (set-macro-character #\" 'double-quote nil readtable)
  (set-macro-character #\; 'semicolon nil readtable)
  (set-macro-character #\` 'backquote nil readtable)
  (set-macro-character #\, 'comma nil readtable)

  (set-dispatch-macro-character
   #\# #\' 'sharpsign-single-quote readtable)

  (set-dispatch-macro-character
   #\# #\( 'sharpsign-left-parenthesis readtable)

  (set-dispatch-macro-character
   #\# #\. 'sharpsign-dot readtable)

  (set-dispatch-macro-character
   #\# #\\ 'sharpsign-backslash readtable)

  (set-dispatch-macro-character
   #\# #\b 'sharpsign-b readtable)

  (set-dispatch-macro-character
   #\# #\x 'sharpsign-x readtable)

  (set-dispatch-macro-character
   #\# #\o 'sharpsign-o readtable)

  (set-dispatch-macro-character
   #\# #\r 'sharpsign-r readtable)

  (set-dispatch-macro-character
   #\# #\* 'sharpsign-asterisk readtable)

  (set-dispatch-macro-character
   #\# #\| 'sharpsign-vertical-bar readtable)

  (set-dispatch-macro-character
   #\# #\a 'sharpsign-a readtable)

  (set-dispatch-macro-character
   #\# #\: 'sharpsign-colon readtable)

  (set-dispatch-macro-character
   #\# #\c 'sharpsign-c readtable)

  (set-dispatch-macro-character
   #\# #\p 'sharpsign-p readtable)

  (set-dispatch-macro-character
   #\# #\+ 'sharpsign-plus readtable)

  (set-dispatch-macro-character
   #\# #\- 'sharpsign-minus readtable)

  (set-dispatch-macro-character
   #\# #\< 'sharpsign-invalid readtable)

  (set-dispatch-macro-character
   #\# #\) 'sharpsign-invalid readtable))
