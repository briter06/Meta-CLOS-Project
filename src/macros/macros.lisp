(in-package :closless)

(defmacro defclass (name superclasses slots)
  `(defvar ,name
           (make-class :direct-superclass ,(let ((s (car superclasses))) (if s s '*object*)) :direct-slots ',slots)))