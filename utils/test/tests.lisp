(defmacro assert-equals (x y)
  `(cond
   ((equal ,x ,y) t)
   (t (error (format nil "Assert error in (assert-equals ~d ~d) = Received: ~d | Expected: ~d~%" ',x ',y ,x ,y))))
)