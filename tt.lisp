; (defclass animal () ())
; (defclass dog (animal) ())

; (defclass food () ())
; (defclass apple (food) ())

; (defgeneric display (animal food))

; (defmethod display ((animal animal) (apple apple))
;   (print "Display 1"))

; (defmethod display ((dog dog) (food food))
;   (print "Display 2"))

; (display (make-instance 'animal) (make-instance 'food))

(defclass person () ())
(defclass student (person) ())

(defgeneric greet (o))

(defmethod greet ((o person)) (declare (ignore o)) (print "Person - Main"))
(defmethod greet ((o student)) (declare (ignore o)) (print "Student - Main"))

(defmethod greet :before ((o person))
  (declare (ignore o))
  (print "I'm a person"))
(defmethod greet :after ((o person))
  (declare (ignore o))
  (print "Bye person"))
(defmethod greet :around ((o person))
  (print "Before around Person")
  (call-next-method)
  (print "After around Person"))

(defmethod greet :before ((o student))
  (declare (ignore o))
  (print "I'm a student"))
(defmethod greet :after ((o student))
  (declare (ignore o))
  (print "Bye student"))
(defmethod greet :around ((o student))
  (print "Before around Student")
  (call-next-method)
  (print "After around Student"))


(greet (make-instance 'student))