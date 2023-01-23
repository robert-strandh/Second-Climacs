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
    :initform nil
    :initarg :next-sibling
    :accessor next-sibling)))

(defun develop-children (wad-descriptor)
  (loop with previous = nil
        with wad = (wad wad-descriptor)
        for child in (children wad)
        for start-line = (start-line child)
        for reference = (start-line-number wad-descriptor)
          then (+ reference start-line)
        for child-descriptor
          = (make-instance 'wad-descriptor
              :wad child
              :start-line-number (+ reference start-line)
              :start-column-number (start-column child)
              :end-line-number (+ reference start-line (height child))
              :end-column-number (end-column child)
              :parent wad-descriptor
              :previous-sibling previous)
        do (if (null previous)
               (setf (first-child wad-descriptor) child-descriptor)
               (setf (next-sibling previous) child-descriptor))
           (setf previous child-descriptor)))

;;; Return the top-level wad that contains the cursor, and make sure
;;; that that wad is the first wad of the prefix.  If the cursor is
;;; not contained in any wad, i.e., it is at the top level, then
;;; return NIL, and make sure that every wad of the prefix precedes
;;; the cursor, and every wad of the suffix succeeds the cursor.
(defun find-top-level-wad-containing-cursor (cache cursor)
  (multiple-value-bind (cursor-line-number cursor-column-number)
      (base:cursor-positions cursor)
    (loop until (or (null (prefix cache))
                    (position-is-after-wad-p
                     (first (prefix cache))
                     cursor-line-number
                     cursor-column-number)
                    (position-is-inside-wad-p
                     (first (prefix cache))
                     cursor-line-number
                     cursor-column-number))
          do (push-to-suffix cache (pop-from-prefix cache)))
    (loop until (or (null (suffix cache))
                    (position-is-before-wad-p
                     (first (suffix cache))
                     cursor-line-number
                     cursor-column-number))
          do (push-to-prefix cache (pop-from-suffix cache)))
    (if (null (suffix cache))
        nil
        (first (suffix cache)))))

(defun position-is-inside-interval-p
    (position-line-number
     position-column-number
     interval-start-line-number
     interval-start-column-number
     interval-end-line-number
     interval-end-column-number)
  (and (or (> position-line-number interval-start-line-number)
           (and (= position-line-number interval-start-line-number)
                (> position-column-number interval-start-column-number)))
       (or (< position-line-number interval-end-line-number)
           (and (= position-line-number interval-end-line-number)
                (< position-column-number interval-end-column-number)))))

(defun position-is-before-interval-p
    (position-line-number
     position-column-number
     interval-start-line-number
     interval-start-column-number)
  (or (< position-line-number interval-start-line-number)
      (and (= position-line-number interval-start-line-number)
           (<= position-column-number interval-start-column-number))))

;;; Given the first child of a wad descriptor (which may be NIL) and a
;;; cursor, return three values.  If the cursor is inside one of the
;;; children, say C, then return the left sibling C, C, and the right
;;; sibling of C (or NIL when the sibling does not exist).  Otherwise
;;; return the wad descriptor immediately preceding the cursor, NIL,
;;; and the wad descriptor immediately following the cursor (or NIL
;;; when it doesn't exist).
(defun siblings (first-child cursor)
  (multiple-value-bind (cursor-line-number cursor-column-number)
      (base:cursor-positions cursor)
    (if (null first-child)
        (values nil nil nil)
        (loop for previous = nil then child
              for child = first-child then (next-sibling child)
              until (null child)
              do (cond ((position-is-before-interval-p
                         cursor-line-number
                         cursor-column-number
                         (start-line-number child)
                         (start-column-number child))
                        (return (values nil nil (next-sibling child))))
                       ((position-is-inside-interval-p
                         cursor-line-number
                         cursor-column-number
                         (start-line-number child)
                         (start-column-number child)
                         (end-line-number child)
                         (end-column-number child))
                        (return (values (previous-sibling child)
                                        child
                                        (next-sibling child))))
                       (t nil))
              finally (return (values previous nil nil))))))

;;; Given an absolute wad, return a wad descriptor with the wad and
;;; the interval initialized.
(defun make-wad-descriptor-from-wad (wad)
  (make-instance 'wad-descriptor
    :wad wad
    :start-line-number (start-line wad)
    :start-column-number (start-column wad)
    :end-line-number (+ (start-line wad) (height wad))
    :end-column-number (end-column wad)))
