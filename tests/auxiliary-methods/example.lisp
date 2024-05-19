(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass number () (value))

(defvar num (make-instance 'number))
(setf (slot-value num 'value) 5)

(defgeneric square (x))

(defmethod square (x)
    (* (slot-value x 'value) (slot-value x 'value)))

(defvar logger '())

(defmethod square :before (x)
    (setf logger (append logger (list (concatenate 'string "Before square: " (write-to-string (slot-value x 'value)))))))

(defmethod square :after (x)
    (setf logger (append logger (list (concatenate 'string "After square: " (write-to-string (slot-value x 'value)))))))

(assert-equals (square num) 25)
(assert-equals logger '("Before square: 5" "After square: 5"))

(unbound-variables '(<number> num square logger))

(print "Auxiliary Methods | Assignment example => All the tests passed")