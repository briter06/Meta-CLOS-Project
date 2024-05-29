;; Macro to assert the equality of two expressions
(defmacro assert-equals (x y)
  `(cond
    ((equal ,x ,y) t)
    (t (error (format nil "Assert error in (assert-equals ~d ~d) = Received: ~d | Expected: ~d~%" ',x ',y ,x ,y)))))

;; Macro to check that an expression should raise s simple-error with an specific message
(defmacro assert-should-raise-simple-error (x error-message)
  `(handler-case ,x
     (simple-error (e)
                   (assert-equals (format nil "~a" e) ,error-message))
     (error (e)
       (error (format nil "Assert error in (assert-should-raise-simple-error ~d ~d) | Incorrect error was raised. Received error: ~d" ',x ',error-message e)))
     (:no-error (res) (declare (ignore res))
                (error (format nil "Assert error in (assert-should-raise-simple-error ~d ~d) | Execution did not raise any errors" ',x ',error-message)))))

;; Function to unbound variables so they can be reused
(defun unbound-variables (variables)
  (loop for variable in variables do (makunbound variable) finally (return t)))

;; Function to print a value with indentation and a '> symbol
(defun print-command-result (x)
  (format t "~2a> ~d~%" "" x)
  x)

;; Function to print a command and also print the result, both with indentation
(defmacro print-command (command)
  (format t "~2a~d~%" "" command)
  `(print-command-result ,command))

;; Function to generate code for adding a message to a logger, where the logger is just a list of messages
(defun add-log-helper (logger message)
  `(setf ,logger (append ,logger (list ,message))))

;; Macro to add a message to the logger
(defmacro add-log (logger message)
  (add-log-helper logger message))

;; Macro add a message into the logger but also print it
(defmacro log-msg (logger message)
  `(progn
    (print-command-result ,message)
    ,(add-log-helper logger message)))