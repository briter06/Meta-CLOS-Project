(defmacro assert-equals (x y)
  `(cond
    ((equal ,x ,y) t)
    (t (error (format nil "Assert error in (assert-equals ~d ~d) = Received: ~d | Expected: ~d~%" ',x ',y ,x ,y)))))

(defmacro assert-should-raise (x error-type error-message)
  `(handler-case ,x
     (,error-type (e)
       (assert-equals (message e) ,error-message))
     (error (e)
       (error (format nil "Assert error in (assert-should-raise ~d ~d ~d) | Incorrect error was raised. Received error: ~d" ',x ',error-type ',error-message e)))
     (:no-error (res) (declare (ignore res))
                (error (format nil "Assert error in (assert-should-raise ~d ~d ~d) | Execution did not raise any errors" ',x ',error-type ',error-message)))))

(defmacro unbound-classes (classes)
  (loop for class in classes do (makunbound class) finally (return t)))