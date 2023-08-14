(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-incremental-parsing
  (:use #:common-lisp)
  (:shadow #:package-name #:symbol-name)
  (:local-nicknames (#:reader #:eclector.reader)
                    (#:readtable #:eclector.readtable)
                    (#:gs #:trivial-gray-streams)
                    (#:flx #:flexichain))
  (:export))
