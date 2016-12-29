(cl:in-package #:second-climacs-base)

;;; A BUFFER is an object that holds data to be operated on and
;;; displayed.  An instance of this class typically contains a
;;; reference to another object, perhaps provided by an external
;;; library.
;;;
;;; A typical situation would be that a subclass of BUFFER contains a
;;; slot holding a reference to a Cluffer buffer object.

(defclass buffer ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INSERT-ITEM.
;;;
;;; Take a cursor and an iten, and insert the item at cursor.

(defgeneric insert-item (cursor item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DELETE-ITEM-.
;;;
;;; Delete the item immediately after the cursor.

(defgeneric delete-item (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ERASE-ITEM-.
;;;
;;; Delete the item immediately before the cursor.

(defgeneric erase-item (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FORWARD-ITEM-.
;;;
;;; Move the cursor forward one item.

(defgeneric forward-item (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BACKWARD-ITEM-.
;;;
;;; Move the cursor backward one item.

(defgeneric backward-item (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BEGINNING-OF-LINE.
;;;
;;; Move the cursor to the beginning of the current line.

(defgeneric beginning-of-line (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function END-OF-LINE.
;;;
;;; Move the cursor to the end of the current line.

(defgeneric end-of-line (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ITEM-BEFORE-CURSOR.
;;;
;;; Return the item immediately preceding the cursor.

(defgeneric item-before-cursor (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ITEM-AFTER-CURSOR.
;;;
;;; Return the item immediately preceding the cursor.

(defgeneric item-after-cursor (cursor))
