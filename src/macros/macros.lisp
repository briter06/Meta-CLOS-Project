(in-package :closless)

(defmacro defclass (name superclasses slots)
  `(defvar ,name
           (make-class :direct-superclass (append ',superclasses '(*object*)) :direct-slots ',slots)))