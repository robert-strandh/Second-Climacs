(cl:in-package #:trucler-native)

(defmethod trucler:get-setf-expansion (client environment place)
  (get-setf-expansion place))
