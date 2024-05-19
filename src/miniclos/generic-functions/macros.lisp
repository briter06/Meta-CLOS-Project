(in-package :closless)

(defmacro defgeneric (name)
  `(defvar ,name (make-generic-function)))