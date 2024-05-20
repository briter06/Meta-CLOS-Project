(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass person () ())
(defclass student (person) ())

(defvar logger '())

(defgeneric greet (o))

(defmethod greet ((o person))
  (declare (ignore o))
  (setf logger (append logger '("Person - Main")))
  "Person - Main")
(defmethod greet ((o student))
  (declare (ignore o))
  (setf logger (append logger '("Student - Main")))
  "Student - Main")

(defmethod greet :before ((o person))
  (declare (ignore o))
  (setf logger (append logger '("I'm a person"))))
(defmethod greet :after ((o person))
  (declare (ignore o))
  (setf logger (append logger '("Bye person"))))

(defmethod greet :around ((o person))
  (declare (ignore o))
  (setf logger (append logger '("Before around Person")))
  (let ((result (call-next-method)))
    (setf logger (append logger '("After around Person")))
    (concatenate 'string result " -> Around Person")))

(defmethod greet :before ((o student))
  (declare (ignore o))
  (setf logger (append logger '("I'm a student"))))
(defmethod greet :after ((o student))
  (declare (ignore o))
  (setf logger (append logger '("Bye student"))))

(defmethod greet :around ((o student))
  (declare (ignore o))
  (setf logger (append logger '("Before around Student")))
  (let ((result (call-next-method)))
    (setf logger (append logger '("After around Student")))
    (concatenate 'string result " -> Around Student")))

(assert-equals (greet (make-instance 'student)) "Student - Main -> Around Person -> Around Student")
(assert-equals logger
               '("Before around Student"
                 "Before around Person"
                 "I'm a student"
                 "I'm a person"
                 "Student - Main"
                 "Bye person"
                 "Bye student"
                 "After around Person"
                 "After around Student"))

(setf logger '())
(assert-equals (greet (make-instance 'person)) "Person - Main -> Around Person")
(assert-equals logger
               '("Before around Person"
                 "I'm a person"
                 "Person - Main"
                 "Bye person"
                 "After around Person"))

(unbound-variables '(<person> <student> greet logger))

(print "Auxiliary Methods => All the tests passed")