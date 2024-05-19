(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass person () ())
(defclass student (person) ())

(defvar logger '())

(defgeneric greet (o))

(defmethod greet ((o person)) (declare (ignore o)) "Person - Main")
(defmethod greet ((o student)) (declare (ignore o)) "Student - Main")

(defmethod greet :before ((o person))
    (declare (ignore o))
    (setf logger (append logger '("I'm a person"))))
; (defmethod greet :after ((o person)) (print "Bye person"))

(defmethod greet :before ((o student))
    (declare (ignore o))
    (setf logger (append logger '("I'm a student"))))
; (defmethod greet :after ((o student)) (print "Bye student"))

(assert-equals (greet (make-instance 'student)) "Student - Main")
(assert-equals logger '("I'm a student" "I'm a person"))

(unbound-variables '(<person> <student> greet))