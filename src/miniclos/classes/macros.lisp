(in-package :closless)

(defmacro defclass (name superclasses slots)
  `(defvar ,(mangle-class-name name)
           (make-class
            :name-symbol ',(mangle-class-name name)
            :direct-superclasses ,(if superclasses (cons 'list (mapcar #'mangle-class-name superclasses)) '(list *object*))
            :direct-slots ',slots)))