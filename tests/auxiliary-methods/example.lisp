(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass number () (value))

(defgeneric square (x))

(defmethod square (x)
  (* (slot-value x 'value) (slot-value x 'value)))

(defvar logger '())

(defmethod square :before (x)
  (setf logger (append logger (list (concatenate 'string "About to calculate the square of " (write-to-string (slot-value x 'value)))))))

(defmethod square :after (x)
  (setf logger (append logger (list (concatenate 'string "Just calculated the square of " (write-to-string (slot-value x 'value)))))))

(defvar num (make-instance 'number))
(setf (slot-value num 'value) 5)
(assert-equals (square num) 25)
(assert-equals logger '("About to calculate the square of 5"
                        "Just calculated the square of 5"))

(setf logger '())

(defgeneric multiply (x y))

(defmethod multiply (x y)
  (* (slot-value x 'value) (slot-value y 'value)))

(defmethod multiply :around (x y)
  (declare (ignore x))
  (declare (ignore y))
  (setf logger (append logger '("Before multiplying")))
  (let ((result (call-next-method)))
    (setf logger (append logger '("After multiplying")))
    result))

(defvar num1 (make-instance 'number))
(setf (slot-value num1 'value) 3)
(defvar num2 (make-instance 'number))
(setf (slot-value num2 'value) 5)
(assert-equals (multiply num1 num2) 15)
(assert-equals logger '("Before multiplying"
                        "After multiplying"))

(unbound-variables '(<number> num num1 num2 square multiply logger))

(print "Auxiliary Methods | Assignment example => All the tests passed")