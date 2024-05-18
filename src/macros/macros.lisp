(in-package :closless)

(defmacro defclass (name superclasses slots)
  `(defvar ,name
           (make-class :direct-superclass ,(append superclasses (list object)) :direct-slots ',slots)))