(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass number () (value))

(defgeneric square (x))

(defmethod square (x)
  (* (slot-value x 'value) (slot-value x 'value)))

(defvar logger '())

(defmethod square :before (x)
  (log-msg logger (concatenate 'string "About to calculate the square of " (write-to-string (slot-value x 'value)))))

(defmethod square :after (x)
  (log-msg logger (concatenate 'string "Just calculated the square of " (write-to-string (slot-value x 'value)))))

(format t "Auxiliary Methods | Assignment example | Square => Start testing~%")
(format t "~%")

(print-command (defvar num (make-instance 'number)))
(format t "~%")
(print-command (setf (slot-value num 'value) 5))
(format t "~%")
(assert-equals (print-command (square num)) 25)
(format t "~%")
(assert-equals logger '("About to calculate the square of 5"
                        "Just calculated the square of 5"))

(format t "Auxiliary Methods | Assignment example | Square => All the tests passed~%")
(format t "~%")

(setf logger '())

(defgeneric multiply (x y))

(defmethod multiply (x y)
  (* (slot-value x 'value) (slot-value y 'value)))

(defmethod multiply :around (x y)
  (declare (ignore x))
  (declare (ignore y))
  (log-msg logger "Before multiplying")
  (let ((result (call-next-method)))
    (log-msg logger "After multiplying")
    result))

(format t "Auxiliary Methods | Assignment example | Multiply => Start testing~%")
(format t "~%")

(print-command (defvar num1 (make-instance 'number)))
(format t "~%")
(print-command (setf (slot-value num1 'value) 3))
(format t "~%")
(print-command (defvar num2 (make-instance 'number)))
(format t "~%")
(print-command (setf (slot-value num2 'value) 5))
(format t "~%")
(assert-equals (print-command (multiply num1 num2)) 15)
(format t "~%")
(assert-equals logger '("Before multiplying"
                        "After multiplying"))

(unbound-variables '(<number> num num1 num2 square multiply logger))

(format t "Auxiliary Methods | Assignment example | Multiply => All the tests passed~%")
(format t "~%")