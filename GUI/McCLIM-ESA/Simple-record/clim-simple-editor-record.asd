(cl:in-package #:asdf-user)

;;;; This system defines a new CLIM top-level output record suitable
;;;; for simple presentation of an editor buffer contents.  It is
;;;; simple, in that it does not attempt to fold lines as a function
;;;; of the width of the visible area of the pane.  Instead it relies
;;;; on horizontal scrolling.
;;;;
;;;; This top-level output record is considered to have
;;;; non-overlapping child records, each representing a line of editor
;;;; text (or other displayable items).
;;;;
;;;; The essential characteristic of this top-level output record is
;;;; that individual child records can be inserted or removed in a
;;;; very efficient way, at least as long as these operations have
;;;; some locality, so that it is likely that an operation is close to
;;;; the previous operation.  Furthermore, computing the new width and
;;;; height of the output record after an operation is also very
;;;; efficient.
;;;;
;;;; The essential technique for accomplishing this performance is to
;;;; represent the sequence of child output records as a splay tree.
;;;; Each node of the splay tree contains summary information about
;;;; the dimensions of its children.  Nodes in the tree that are
;;;; recently modified automatically migrate close to the root of the
;;;; splay tree, which is how we take advantage of the likely locality
;;;; scenario.

(defsystem clim-simple-editor-record
  :depends-on (:mcclim :clump)
  :serial t
  :components
  ((:file "packages")
   (:file "record")
   (:file "node")
   (:file "line-output-records")))
