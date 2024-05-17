(in-package :closless)

(defun make-instance (class)
  (make-object :class (symbol-value class)))