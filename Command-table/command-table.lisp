(cl:in-package #:climacs-command-table)

(defclass command-table ()
  ())

(defclass basic-command-table (command-table)
  ((%map :initform (make-instance 'ducling:map :test #'equal))))

(defclass aggregate-command-table (command-table)
  ((%command-tables :initarg :command-tables :reader command-tables)))
