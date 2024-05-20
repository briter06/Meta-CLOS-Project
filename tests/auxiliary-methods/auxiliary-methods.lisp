(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass person () ())
(defclass student (person) ())

(defvar logger '())

(defgeneric run (o))

(defmethod run ((o person))
  (declare (ignore o))
  (add-log logger "Person - Main")
  "Person - Main")
(defmethod run ((o student))
  (declare (ignore o))
  (add-log logger "Student - Main")
  "Student - Main")

(defmethod run :before ((o person))
  (declare (ignore o))
  (add-log logger "I'm a person"))
(defmethod run :after ((o person))
  (declare (ignore o))
  (add-log logger "Bye person"))

(defmethod run :around ((o person))
  (declare (ignore o))
  (add-log logger "Before around Person")
  (let ((result (call-next-method)))
    (add-log logger "After around Person")
    (concatenate 'string result " -> Around Person")))

(defmethod run :before ((o student))
  (declare (ignore o))
  (add-log logger "I'm a student"))
(defmethod run :after ((o student))
  (declare (ignore o))
  (add-log logger "Bye student"))

(defmethod run :around ((o student))
  (declare (ignore o))
  (add-log logger "Before around Student")
  (let ((result (call-next-method)))
    (add-log logger "After around Student")
    (concatenate 'string result " -> Around Student")))

(assert-equals (run (make-instance 'student)) "Student - Main -> Around Person -> Around Student")
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
(assert-equals (run (make-instance 'person)) "Person - Main -> Around Person")
(assert-equals logger
               '("Before around Person"
                 "I'm a person"
                 "Person - Main"
                 "Bye person"
                 "After around Person"))

(unbound-variables '(<person> <student> run logger))

(format t "Auxiliary Methods => All the tests passed~%")