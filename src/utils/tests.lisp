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

(defun unbound-variables (variables)
  (loop for variable in variables do (makunbound variable) finally (return t)))

(defun print-command-result (x)
  (format t "~2a> ~d~%" "" x)
  x)

(defmacro print-command (command)
  (format t "~2a~d~%" "" command)
  `(print-command-result ,command))

(defun add-log-helper (logger message)
  `(setf ,logger (append ,logger (list ,message))))

(defmacro add-log (logger message)
  (add-log-helper logger message))

(defmacro log-msg (logger message)
  `(progn
    (print-command-result ,message)
    ,(add-log-helper logger message)))