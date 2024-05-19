(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass number () (value))

(defvar num (make-instance 'number))
(setf (slot-value num 'value) 5)

(defgeneric square (x))

(defmethod square (x)
    (* (slot-value x 'value) (slot-value x 'value)))

; (defmethod square :before (x)
;     (print "Before square"))

(print (square num))