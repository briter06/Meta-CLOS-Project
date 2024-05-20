(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

;; Scenario 1 | EQL specializer

(defclass city () (population))
(defvar paris (make-instance 'city))
(setf (slot-value paris 'population) 2.161)
(defvar brussels (make-instance 'city))
(setf (slot-value brussels 'population) 1.209)

(defgeneric population (city))

(defmethod population ((city city))
  (slot-value city 'population))

(defmethod population ((city (eql brussels)))
  (declare (ignore city))
  0)

(assert-equals (population paris) 2.161)
(assert-equals (population brussels) 0)

(unbound-variables '(<city> paris brussels population))

;; Scenario 2 | EQL specializer 2

(format t "EQL Specializer | Animals => Start testing~%")
(format t "~%")

(defclass animal () ())
(defclass dog (animal) ())

(defclass food () ())
(defclass apple (food) ())

(defclass person () ())
(defclass student (person) ())

(defvar *super-apple* (make-instance 'apple))
(defvar *super-person* (make-instance 'person))

(defgeneric display-eql (a f p))

(defmethod display-eql ((d dog) (a apple) (p person))
  (declare (ignore d))
  (declare (ignore a))
  (declare (ignore p))
  "Method 1")

(defmethod display-eql ((d dog) (a apple) (s student))
  (declare (ignore d))
  (declare (ignore a))
  (declare (ignore s))
  (concatenate 'string "Method 2 -> " (call-next-method)))

(defmethod display-eql ((d dog) (a (eql *super-apple*)) (p person))
  (declare (ignore d))
  (declare (ignore a))
  (declare (ignore p))
  (concatenate 'string "Super Apple Method -> " (call-next-method)))

(defmethod display-eql ((d dog) (a apple) (p (eql *super-person*)))
  (declare (ignore d))
  (declare (ignore a))
  (declare (ignore p))
  (concatenate 'string "Super Person Method -> " (call-next-method)))

(assert-equals (print-command (display-eql (make-instance 'dog) (make-instance 'apple) (make-instance 'student))) "Method 2 -> Method 1")
(format t "~%")
(assert-equals (print-command (display-eql (make-instance 'dog) (make-instance 'apple) (make-instance 'person))) "Method 1")
(format t "~%")
(assert-equals (print-command (display-eql (make-instance 'dog) *super-apple* (make-instance 'person))) "Super Apple Method -> Method 1")
(format t "~%")
(assert-equals (print-command (display-eql (make-instance 'dog) (make-instance 'apple) *super-person*)) "Super Person Method -> Method 1")
(format t "~%")
(assert-equals (print-command (display-eql (make-instance 'dog) *super-apple* *super-person*)) "Super Apple Method -> Super Person Method -> Method 1")
(format t "~%")

(unbound-variables '(<animal> <dog> <food> <apple> display-eql *super-apple* *super-person*))

(format t "EQL Specializer | Animals => All the tests passed~%")
(format t "~%")