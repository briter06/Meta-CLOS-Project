(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass number () (value))

(defvar num (make-instance 'number))
(setf (slot-value num 'value) 5)

(defgeneric square (x))

(defmethod square (x)
    (* (slot-value x 'value) (slot-value x 'value)))

(defvar counter 0)

(defmethod square :before (x)
    (declare (ignore x))
    (setf counter (+ 1 counter)))

(assert-equals (square num) 25)
(assert-equals counter 1)

(unbound-variables '(<number> num square))

(print "Auxiliary Methods | Assignment example => All the tests passed")