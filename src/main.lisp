(load "src/macros/macros.lisp")
(load "src/functions/functions.lisp")

; Tests

(load "src/tests/precedence-list.lisp")

; ; (defvar person
; ;     (make-class :direct-slots '(name age)))

; (defclass person () (name age))

; ; (defparameter *person* (make-object :class person))
; (defparameter *person* (make-instance 'person))

; (setf (slot-value *person* 'name) "Briter")
; (setf (slot-value *person* 'age) 23)


; (print (slot-value *person* 'name))
; (print (slot-value *person* 'age))