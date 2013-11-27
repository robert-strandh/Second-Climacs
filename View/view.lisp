(cl:in-package #:climacs-view)

(defclass view ()
  ((%analyzer :initarg :analyzer :reader analyzer)
   (%show :initarg :show :accessor show)
   (%command-key-processor
    :initarg :command-key-processor
    :reader clim3:command-table
    :accessor command-key-processor)
   (%cursor :initarg :cursor :reader cursor)))

(defmethod clim3:acquire-action :around ((view view))
  (let ((clim3:*key-handler*
	  (make-instance 'clim3:key-processor-key-handler
	    :processor (command-key-processor view))))
    (call-next-method)))

  

