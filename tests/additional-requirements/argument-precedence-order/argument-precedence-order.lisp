(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass animal () ())
(defclass dog (animal) ())

(defclass food () ())
(defclass apple (food) ())

(defclass location () ())
(defclass city (location) ())

(format t "ARGUMENT PRECEDENCE ORDER:~%")
(format t "~%")

;; Scenario 2 | Different order

(format t "Argument Precedence Order | Default order => Start testing~%")
(format t "~%")

(defgeneric display-order (a f l))

(defmethod display-order ((a animal) (ap apple) (l location))
  (declare (ignore a))
  (declare (ignore ap))
  (declare (ignore l))
  "Method 1")

(defmethod display-order ((d dog) (f food) (l location))
  (declare (ignore d))
  (declare (ignore f))
  (declare (ignore l))
  (concatenate 'string "Method 2 -> " (call-next-method)))

(defmethod display-order ((a animal) (f food) (c city))
  (declare (ignore a))
  (declare (ignore f))
  (declare (ignore c))
  (concatenate 'string "Method 3 -> " (call-next-method)))

(assert-equals (print-command (display-order (make-instance 'dog) (make-instance 'apple) (make-instance 'city))) "Method 2 -> Method 1")
(format t "~%")
(assert-equals (print-command (display-order (make-instance 'dog) (make-instance 'apple) (make-instance 'location))) "Method 2 -> Method 1")
(format t "~%")

(format t "Argument Precedence Order | Default order => All the tests passed~%")
(format t "~%")

;; Scenario 2 | Different order

(format t "Argument Precedence Order | Different order => Start testing~%")
(format t "~%")

(defgeneric display-diff-order (a f l) (:argument-precedence-order (l a f)))

(defmethod display-diff-order ((a animal) (ap apple) (l location))
  (declare (ignore a))
  (declare (ignore ap))
  (declare (ignore l))
  "Method 1")

(defmethod display-diff-order ((d dog) (f food) (l location))
  (declare (ignore d))
  (declare (ignore f))
  (declare (ignore l))
  (concatenate 'string "Method 2 -> " (call-next-method)))

(defmethod display-diff-order ((a animal) (f food) (c city))
  (declare (ignore a))
  (declare (ignore f))
  (declare (ignore c))
  (concatenate 'string "Method 3 -> " (call-next-method)))

(assert-equals (print-command (display-diff-order (make-instance 'dog) (make-instance 'apple) (make-instance 'city))) "Method 3 -> Method 2 -> Method 1")
(format t "~%")
(assert-equals (print-command (display-diff-order (make-instance 'dog) (make-instance 'apple) (make-instance 'location))) "Method 2 -> Method 1")
(format t "~%")

(unbound-variables '(<animal> <dog> <food> <apple> <location> <city> display-order display-diff-order))

(format t "Argument Precedence Order | Different order => All the tests passed~%")
(format t "----------------------------------------------------------------------------------~%")
(format t "~%")