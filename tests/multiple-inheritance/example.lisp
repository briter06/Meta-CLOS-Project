(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass Person () ())
(defclass Staff (Person) ())
(defclass Student (Person) ())
(defclass PhDStudent (Student Staff) ())

(defgeneric study (p))
(defmethod study ((p Person)) "I'm not a student.")
(defmethod study ((p Student)) "Let's study.")
(defmethod study ((p Staff))
    (concatenate 'string "I work with students, but " (call-next-method)))

(defgeneric greet (p))
(defmethod greet ((p Person))
    "How are you?")
(defmethod greet ((p Staff))
    (concatenate 'string "Time is ticking. " (call-next-method)))
(defmethod greet ((p Student))
    (concatenate 'string "Deadlines? Deadlines! " (call-next-method)))
(defmethod greet ((p PhDStudent))
    (concatenate 'string "Let's research! " (call-next-method)))

(defvar phdstudent-obj (make-instance 'PhDStudent))
(defvar person-obj (make-instance 'Person))
(defvar staff-obj (make-instance 'Staff))

; (unbound-variables (Person Staff Student PhDStudent study greet phdstudent person staff))

(print "Generic Functions | Multiple Inheritance | Assignment example => All the tests passed")