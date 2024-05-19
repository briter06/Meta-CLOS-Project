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

(defun custom-function (&rest args)
  (print args)
  ; (if before-supplied-p before (car args))
  )