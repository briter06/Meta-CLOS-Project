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

(defgeneric display (object))

(assert-should-raise (defmethod display ((person person) arg2) t) generic-function-error "Invalid number of arguments: 2")

(defmethod display ((person person))
  (declare (ignore person))
  (error 'generic-function-error :message "This method should be overriden"))

(assert-should-raise (display p) generic-function-error "This method should be overriden")

(defmethod display ((person person))
  `(,(slot-value person 'name) ,(slot-value person 'address)))

(defmethod display ((employee employee))
  (cons (slot-value employee 'employer) (call-next-method)))

(assert-equals (display p) '("Briter" "Brussels"))
(assert-equals (display e) '("VUB" "Andres" "Ghent"))

(unbound-variables '(<person> <employee> p e display))

(print "Generic Functions => All the tests passed")