(defmacro defclass (name superclasses slots)
    `(defvar ,name
        (make-class :direct-superclass ,(let ((s (car superclasses))) (if s s 'object)) :direct-slots ',slots)))

(defun make-instance (class)
    (make-object :class (symbol-value class)))