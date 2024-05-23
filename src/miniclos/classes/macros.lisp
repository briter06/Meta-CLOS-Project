(in-package :closless)

(defmacro defclass (name superclasses slots)
  (let ((mangled-name (mangle-class-name name)))
    `(defvar ,mangled-name
             (make-class
              :name-symbol ',mangled-name
              :direct-superclasses
              ,(if superclasses (cons 'list (mapcar #'mangle-class-name superclasses)) '(list *object*))
              :direct-slots ',slots))))