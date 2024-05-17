(in-package :closless)

(defun make-instance (class)
  (make-object :class (symbol-value class)))


(defun generate-class-set (class)
  (class-direct-superclass class))