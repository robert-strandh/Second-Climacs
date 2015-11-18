(cl:in-package #:common-lisp-user)

(defpackage #:splay-tree
  (:use #:common-lisp)
  (:export
   ;; The name of the class for nodes of the splay tree.
   #:node
   ;; The name of the operation for splaying a node.
   #:splay
   ;; The name of the accessor for obtaining or changing the left
   ;; child of a node.
   #:left
   ;; The name of the accessor obtaining or changing the right child
   ;; of a node.
   #:right
   ;; The name of a generic function for obtaining the parent of a
   #:parent))
