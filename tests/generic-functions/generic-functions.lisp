(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

;; Scenario 1 | Simple dispatch

(defclass person () (name address))
(defclass employee (person) (employer))

(defparameter person-obj (make-instance 'person))
(setf (slot-value person-obj 'name) "Briter")
(setf (slot-value person-obj 'address) "Brussels")

(defparameter employee-obj (make-instance 'employee))
(setf (slot-value employee-obj 'name) "Andres")
(setf (slot-value employee-obj 'address) "Ghent")
(setf (slot-value employee-obj 'employer) "VUB")

(defgeneric display (object))

(assert-should-raise (defmethod display ((person person) arg2) t) generic-function-error "Invalid number of arguments: 2")

(defmethod display ((person person))
  (declare (ignore person))
  (error 'generic-function-error :message "This method should be overriden"))

(assert-should-raise (display person-obj) generic-function-error "This method should be overriden")

(defmethod display ((person person))
  `(,(slot-value person 'name) ,(slot-value person 'address)))

(defmethod display ((employee employee))
  (cons (slot-value employee 'employer) (call-next-method)))

(assert-equals (display person-obj) '("Briter" "Brussels"))
(assert-equals (display employee-obj) '("VUB" "Andres" "Ghent"))

(unbound-variables '(<person> <employee> person-obj employee-obj display))

;; Scenario 2 | Inner precedence

(defclass animal () ())
(defclass dog (animal) ())

(defclass food () ())
(defclass apple (food) ())

(defgeneric display (animal food))

(defmethod display ((animal animal) (apple apple))
  (declare (ignore animal))
  (declare (ignore apple))
  "Method 1")

(defmethod display ((dog dog) (food food))
  (declare (ignore dog))
  (declare (ignore food))
  (concatenate 'string "Method 2 -> " (call-next-method)))

(defmethod display ((dog dog) (food apple))
  (declare (ignore dog))
  (declare (ignore food))
  (concatenate 'string "Method 3 -> " (call-next-method)))

(assert-equals (display (make-instance 'dog) (make-instance 'apple)) "Method 3 -> Method 2 -> Method 1")
(assert-equals (display (make-instance 'animal) (make-instance 'apple)) "Method 1")

(unbound-variables '(<animal> <dog> <food> <apple> display))

;; Scenario 3 | Inner precedence 2

(defclass animal () ())
(defclass dog (animal) ())

(defclass food () ())
(defclass apple (food) ())

(defclass person () ())
(defclass student (person) ())

(defgeneric display (animal food person))

(defmethod display ((dog dog) (apple apple) (person person))
  (declare (ignore dog))
  (declare (ignore apple))
  (declare (ignore person))
  "Method 1")

(defmethod display ((dog dog) (apple apple) (student student))
  (declare (ignore dog))
  (declare (ignore apple))
  (declare (ignore student))
  (concatenate 'string "Method 2 -> " (call-next-method)))

(assert-equals (display (make-instance 'dog) (make-instance 'apple) (make-instance 'student)) "Method 2 -> Method 1")
(assert-equals (display (make-instance 'dog) (make-instance 'apple) (make-instance 'person)) "Method 1")

(unbound-variables '(<animal> <dog> <food> <apple> display))

;; Scenario 4 | EQL specializer

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

;; Scenario 5 | EQL specializer 2

(defclass animal () ())
(defclass dog (animal) ())

(defclass food () ())
(defclass apple (food) ())

(defclass person () ())
(defclass student (person) ())

(defvar *super-apple* (make-instance 'apple))
(defvar *super-person* (make-instance 'person))

(defgeneric display (animal food person))

(defmethod display ((dog dog) (apple apple) (person person))
  (declare (ignore dog))
  (declare (ignore apple))
  (declare (ignore person))
  "Method 1")

(defmethod display ((dog dog) (apple apple) (student student))
  (declare (ignore dog))
  (declare (ignore apple))
  (declare (ignore student))
  (concatenate 'string "Method 2 -> " (call-next-method)))

(defmethod display ((dog dog) (apple (eql *super-apple*)) (person person))
  (declare (ignore dog))
  (declare (ignore apple))
  (declare (ignore person))
  (concatenate 'string "Super Apple Method -> " (call-next-method)))

(defmethod display ((dog dog) (apple apple) (person (eql *super-person*)))
  (declare (ignore dog))
  (declare (ignore apple))
  (declare (ignore person))
  (concatenate 'string "Super Person Method -> " (call-next-method)))

(assert-equals (display (make-instance 'dog) (make-instance 'apple) (make-instance 'student)) "Method 2 -> Method 1")
(assert-equals (display (make-instance 'dog) (make-instance 'apple) (make-instance 'person)) "Method 1")
(assert-equals (display (make-instance 'dog) *super-apple* (make-instance 'person)) "Super Apple Method -> Method 1")
(assert-equals (display (make-instance 'dog) (make-instance 'apple) *super-person*) "Super Person Method -> Method 1")
(assert-equals (display (make-instance 'dog) *super-apple* *super-person*) "Super Apple Method -> Super Person Method -> Method 1")

(unbound-variables '(<animal> <dog> <food> <apple> display *super-apple* *super-person*))

(format t "Generic Functions => All the tests passed~%")