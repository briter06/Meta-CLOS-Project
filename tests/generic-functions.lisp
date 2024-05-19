(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

;; Scenario 1 | Simple dispatch

(defclass person () (name address))
(defclass employee (person) (employer))

(defparameter p (make-instance 'person))
(setf (slot-value p 'name) "Briter")
(setf (slot-value p 'address) "Brussels")

(defparameter e (make-instance 'employee))
(setf (slot-value e 'name) "Andres")
(setf (slot-value e 'address) "Ghent")
(setf (slot-value e 'employer) "VUB")

(defgeneric display)

(defmethod display ((person-obj person))
  `(,(slot-value person-obj 'name) ,(slot-value person-obj 'address)))

(defmethod display ((employee-obj employee))
  (cons (slot-value employee-obj 'employer) (call-next-method)))

(assert-equals (display p) '("Briter" "Brussels"))
(assert-equals (display e) '("VUB" "Andres" "Ghent"))

(unbound-variables (person employee p e display))

(print "Generic Functions => All the tests passed")