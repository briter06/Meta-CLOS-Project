(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

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
  (print (slot-value person-obj 'name))
  (print (slot-value person-obj 'address)))

(defmethod display ((employee-obj employee))
  (call-next-method)
  (print (slot-value employee-obj 'employer)))

(call-generic-function display p)
(call-generic-function display e)