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

; (add-method display
;             (make-method
;              :specializers (list person)
;              :function (lambda (arguments next-methods)
;                          (declare (ignore next-methods))
;                          (print (slot-value (car arguments) 'name))
;                          (print (slot-value (car arguments) 'address)))))

(defmethod display ((person-obj person))
  (print (slot-value person-obj 'name))
  (print (slot-value person-obj 'address)))

; (add-method display
;             (make-method
;              :specializers (list employee)
;              :function (lambda (arguments next-methods)
;                          (let ((next-most-specific-method
;                                 (select-most-specific-method arguments next-methods)))
;                            (funcall (method-function next-most-specific-method)
;                              arguments
;                              (remove next-most-specific-method next-methods)))
;                          (print (slot-value (car arguments) 'employer)))))

(call-generic-function display p)
(call-generic-function display e)