(in-package :closless)

(defmacro defclass (name superclasses slots)
  `(defvar ,name
           (make-class :name-symbol ',name :direct-superclasses (append ,(cons 'list superclasses) (list *object*)) :direct-slots ',slots)))