(in-package :closless)

;; Macro to create a class
(defmacro defclass (name superclasses slots)
  ;; Mangle the name of the class
  (let ((mangled-name (mangle-class-name name)))
    ;; Define a new instance of the class struct
    `(defvar ,mangled-name
             (make-class
              ;; Assign the mangle name
              :name-symbol ',mangled-name
              ;; Mangle the name of the superclasses (the symbol is stored instead of the object itself)
              :direct-superclasses ,(if superclasses (cons 'list (mapcar #'mangle-class-name superclasses)) '(list *object*))
              ;; Assign the slots
              :direct-slots ',slots))))