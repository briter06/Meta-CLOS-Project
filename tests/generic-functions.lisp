(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")

(defclass person () (name address))
(defclass employee (person) (employer))

(defparameter p (make-instance 'person))
(setf (slot-value p 'name) "Briter")
(setf (slot-value p 'address) "Brussels")

(defparameter e (make-instance 'employee))
(setf (slot-value e 'name) "Andres")
(setf (slot-value e 'address) "Ghent")
(setf (slot-value e 'employer) "VUB")

(defgeneric display)

(add-method display
            (make-method
             :specializer person
             :function (lambda (receiver other-args next-methods)
                         (declare (ignore next-methods))
                         (declare (ignore other-args))
                         (print (slot-value receiver 'name))
                         (print (slot-value receiver 'address)))))

(add-method display
            (make-method
             :specializer employee
             :function (lambda (receiver other-args next-methods)
                         (let ((next-most-specific-method
                                (select-most-specific-method next-methods)))
                           (funcall (method-function next-most-specific-method)
                                    receiver
                                    other-args
                                    (remove next-most-specific-method next-methods)))
                         (print (slot-value receiver 'employer)))))

(call-generic-function display p)
(call-generic-function display e)