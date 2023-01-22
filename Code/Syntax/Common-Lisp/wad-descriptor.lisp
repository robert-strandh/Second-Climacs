(cl:in-package #:second-climacs-syntax-common-lisp)

;;;; A wad descriptor is a object that facilitates the manipulation of
;;;; a hierarchy of wads, because it contains absolute positions of
;;;; the start and end of the wad, and it contains references to the
;;;; parent, the children, and the siblings of a wad.  Of course, a
;;;; wad descriptor doesn't stay valid for very long.  As soon as the
;;;; buffer contents changes, it usually becomes invalid.  For that
;;;; reason, a hierarchy of wad descriptors is created only when
;;;; needed by some command, and then thrown away.

(defclass wad-descriptor ()
  (;; This slot contains the wad being described by this wad descriptor.
   (%wad
    :initarg :wad
    :reader wad)
   (%start-line-number
    :initarg :start-line-number
    :reader start-line-number)
   (%start-column-number
    :initarg :start-column-number
    :reader start-column-number)
   (%end-line-number
    :initarg :end-line-number
    :reader end-line-number)
   (%end-column-number
    :initarg :end-column-number
    :reader end-column-number)
   ;; The parent is another wad descriptor.
   (%parent
    :initarg parent
    :reader parent)
   ;; This slot can contain another wad descriptor, which is then a
   ;; descriptor for the first child of the wad described by this wad
   ;; descriptor.  Or it can contain NIL if the the wad described by
   ;; this wad descriptor does not have any children.  Finally, it can
   ;; contain the symbol T which means that wad descriptors for the
   ;; children of the wad described by this wad descriptor have not
   ;; yet been generated.  The slot reader for this slot can not
   ;; return T, because a :BEFORE method on that slot reader will
   ;; generate wad descriptors for the children if they have not yet
   ;; been generated.
   (%first-child
    :initform T
    :accessor first-child)
   ;; This slot can contain another wad descriptor which is then the
   ;; wad descriptor for the previous sibling of the wad being
   ;; described by this wad descriptor.  If the the wad being
   ;; described by this wad descriptor is the first child of its
   ;; parent, then this slot contains NIL.
   (%previous-sibling
    :initarg :previous-sibling
    :reader previous-sibling)
   ;; This slot can contain another wad descriptor which is then the
   ;; wad descriptor for the next sibling of the wad being described
   ;; by this wad descriptor.  If the the wad being described by this
   ;; wad descriptor is the last child of its parent, then this slot
   ;; contains NIL.
   (%next-sibling
    :initarg :next-sibling
    :reader next-sibling)))
