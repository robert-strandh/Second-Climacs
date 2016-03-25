(cl:in-package #:clim-simple-editor-record)

;;; While it is possible to have nodes of the tree contain output
;;; records of any type that is a subclass of the class
;;; RELATIVE-COORDINATES-OUTPUT-RECORD-MIXIN, we provide a few
;;; convenient classes here.

;;; An Instance of this class can be used either directly as the
;;; contents of a leaf tree node, or it can be used as a child output
;;; record of some other type of output record stored in such a leaf.
;;;
;;; For example, for a very simple text editor application, were no
;;; particular information needs to be displayed for individual words,
;;; the entire line of text could be a single instance of this class.
;;; On the other hand, if information such as that provided by a spell
;;; checker should be provided, then each word must be a separate
;;; output record.
(defclass relative-coordinate-text-output-record
    (clim:text-displayed-output-record
     relative-coordinates-output-record-mixin)
  ())

;;; An instance of this class can be used directly as the contents of
;;; a leaf tree node, and in that case, it is expected to have a list
;;; of instances of RELATIVE-COORDINATE-TEXT-OUTPUT-RECORD or some
;;; other relative-coordinate class as its children.  It is not
;;; expected that the contents of an instance of this class will ever
;;; change, so we do not support inserting and deleting children from
;;; it.  For this kind of application, it is simpler to just create a
;;; new instance.
(defclass relative-coordinate-sequence-output-record
    (clim:output-record
     relative-coordinates-output-record-mixin)
  ((%children :initarg :children :reader children)))
