(cl:in-package #:clim-simple-editor-record)

(defclass record (clim:stream-output-history-mixin)
  (;; This slot contains the splay tree that represents the sequence
   ;; of child output records (lines).  If there are no children, then
   ;; this slot contains NIL.
   (%contents :initform nil :initarg :contents :accessor contents)))
