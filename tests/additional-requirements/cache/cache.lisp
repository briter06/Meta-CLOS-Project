(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(format t "CACHE:~%")
(format t "~%")

(format t "Cache | Squared => Start testing~%")
(format t "~%")

(defclass number () (value))

(defvar counter 0)

(defgeneric squared (x) (:cached))
(defmethod squared (x)
  (setf counter (+ 1 counter))
  (* (slot-value x 'value) (slot-value x 'value)))

(print-command (defvar num (make-instance 'number)))
(format t "~%")
(print-command (setf (slot-value num 'value) 5))
(format t "~%")

(assert-equals (print-command (squared num)) 25)
(format t "~%")
(assert-equals (print-command (squared num)) 25)
(format t "~%")

(assert-equals (print-command counter) 1)
(format t "~%")

(format t "Cache | Squared => All the tests passed~%")
(format t "----------------------------------------------------------------------------------~%")
(format t "~%")