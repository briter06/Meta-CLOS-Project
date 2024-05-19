(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass animal () ())
(defclass dog (animal) ())
(defclass cat (animal) ())
(defclass pig (animal) ())

(defclass food () ())
(defclass dog-food (food) ())
(defclass cat-food (food) ())

(defgeneric eat)

(defmethod eat ((animal dog) (food dog-food))
    "I like dog food!")

(defmethod eat ((animal cat) (food cat-food))
    "I like cat food!")

(defmethod eat ((animal dog) (food cat-food))
    "I cannot eat cat food")

(defmethod eat ((animal animal) (food food))
    "I cannot eat this food")

(defmethod eat ((animal animal) food)
    "What you are giving me is not food")

(defvar *my-cat* (make-instance 'cat))
(defvar *my-dog* (make-instance 'dog))
(defvar *my-pig* (make-instance 'pig))
(defvar *cat-food* (make-instance 'cat-food))
(defvar *dog-food* (make-instance 'dog-food))

(assert-equals (eat *my-dog* *dog-food*) "I like dog food!")
(assert-equals (eat *my-cat* *cat-food*) "I like cat food!")
(assert-equals (eat *my-dog* *cat-food*) "I cannot eat cat food")
(assert-equals (eat *my-cat* *dog-food*) "I cannot eat this food")
(assert-equals (eat *my-cat* *my-pig*) "What you are giving me is not food")

(print "Generic Functions | Multiple Dispatch | Assignment example => All the tests passed")