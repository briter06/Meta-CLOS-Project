(in-package :closless)

(defvar <person>
  (make-class :direct-slots '(name address)))

(defvar <employee>
  (make-class :direct-superclass <person>
              :direct-slots '(employer)))

(defvar <display>
  (make-generic-function))

(add-method <display>
            (make-method
             :specializer <person>
             :function (lambda (receiver other-args next-methods)
                         (print (slot-value receiver 'name))
                         (print (slot-value receiver 'address)))))

(add-method <display>
            (make-method
             :specializer <employee>
             :function (lambda (receiver other-args next-methods)
                         (let ((next-most-specific-method
                                (select-most-specific-method next-methods)))
                           (funcall (method-function next-most-specific-method)
                                    receiver
                                    other-args
                                    (remove next-most-specific-method next-methods)))
                         (print (slot-value receiver 'employer)))))

(defun test ()
  (let ((p (make-object :class <person>)))
    (setf (slot-value p 'name) "Pascal")
    (setf (slot-value p 'address) "Brussels")
    (call-generic-function <display> p))

  (let ((p (make-object :class <employee>)))
    (setf (slot-value p 'name) "Pascal")
    (setf (slot-value p 'address) "Brussels")
    (setf (slot-value p 'employer) "VUB")
    (call-generic-function <display> p))

  :done)
