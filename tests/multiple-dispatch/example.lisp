(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(format t "MULTIPLE DISPATCH:~%")
(format t "~%")

(format t "Multiple Dispatch | Assignment example | Animals => Start testing~%")
(format t "~%")

(defclass animal () ())
(defclass dog (animal) ())
(defclass cat (animal) ())
(defclass pig (animal) ())

(defclass food () ())
(defclass dog-food (food) ())
(defclass cat-food (food) ())

(defgeneric eat (animal food))

(defmethod eat ((animal dog) (food dog-food))
  (declare (ignore animal))
  (declare (ignore food))
  "I like dog food!")

(defmethod eat ((animal cat) (food cat-food))
  (declare (ignore animal))
  (declare (ignore food))
  "I like cat food!")

(defmethod eat ((animal dog) (food cat-food))
  (declare (ignore animal))
  (declare (ignore food))
  "I cannot eat cat food")

(defmethod eat ((animal animal) (food food))
  (declare (ignore animal))
  (declare (ignore food))
  "I cannot eat this food")

(defmethod eat ((animal animal) food)
  (declare (ignore animal))
  (declare (ignore food))
  "What you are giving me is not food")

(defvar *my-cat* (make-instance 'cat))
(defvar *my-dog* (make-instance 'dog))
(defvar *my-pig* (make-instance 'pig))
(defvar *cat-food* (make-instance 'cat-food))
(defvar *dog-food* (make-instance 'dog-food))

(assert-equals (print-command (eat *my-dog* *dog-food*)) "I like dog food!")
(format t "~%")

(assert-equals (print-command (eat *my-cat* *cat-food*)) "I like cat food!")
(format t "~%")

(assert-equals (print-command (eat *my-dog* *cat-food*)) "I cannot eat cat food")
(format t "~%")

(assert-equals (print-command (eat *my-cat* *dog-food*)) "I cannot eat this food")
(format t "~%")

(assert-equals (print-command (eat *my-cat* *my-pig*)) "What you are giving me is not food")
(format t "~%")

(unbound-variables '(*my-cat* *my-dog* *my-pig* *cat-food* *dog-food* eat <food> <dog-food> <cat-food> <animal> <dog> <cat> <pig>))

(format t "Multiple Dispatch | Assignment example | Animals => All the tests passed~%")
(format t "----------------------------------------------------------------------------------~%")
(format t "~%")