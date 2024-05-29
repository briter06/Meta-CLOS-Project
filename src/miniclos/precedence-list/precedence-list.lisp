(in-package :closless)
(load "src/utils/sets.lisp")

;; Function for generating a set with all the superclasses of a class (only the symbols) - (Set S in the algorithm)
(defun generate-class-symbol-set (class)
  (mapcar #'class-name-symbol (class-all-superclasses class)))

;; Function for generating couples based on a list
;; For example, for the list '(1 2 3 4 5 6), the result is '((1 2) (2 3) (3 4) (4 5) (5 6))
(defun get-couples (elems)
  (cond
   ((< (length elems) 2) ())
   (t (adjoin (list (car elems) (cadr elems)) (get-couples (cdr elems))))))

;; Function for generating the set with the relations between the classes - (Set R in the algorithm)
(defun generate-class-symbol-relation-set (class)
  ;; Get the symbols of all superclasses
  (let ((symbols (mapcar #'class-name-symbol (class-direct-superclasses class))))
    (cond
     ;; If the class is the class *object*, add the final couple (*object* t)
     ((eql class *object*) '((*object* t)))
     ;; If not, generate the couples for every super class and the current class
     (t (reduce (lambda (acc x)
                  ;; Recursively, generate the couples of the superclasses and add them to the same set
                  (concat-sets (generate-class-symbol-relation-set (symbol-value x)) acc :test #'equal))
            symbols
          ;; Generate the couples for the current class, including the class itself at the beginning and the *object* class at the end
          :initial-value (get-couples (cons (class-name-symbol class) (if (equal symbols '(*object*)) symbols (append symbols '(*object*))))))))))

;; Function to check if a class symbol is present in the right-hand side of any tuple in the set R
(defun is-preceded (class-symbol r)
  (loop for tuple in r when (eql (cadr tuple) class-symbol) do (return t) finally (return nil)))

;; Function to find all the classes that are not preceded by any class in the tuples in the set R
(defun classes-not-preceded (s r)
  (remove-if (lambda (symbol) (is-preceded symbol r)) s))

;; Function to remove the tuples from set R where a specific class symbol appears
(defun remove-apperance-of-class (symbol r)
  (remove-if (lambda (tuple) (or (eql (car tuple) symbol) (eql (cadr tuple) symbol))) r))

;; Function to find the first candiate that is is a direct superclass of a specific class
(defun get-superclass-of-subclass-in-list (class candidates)
  (loop for c in candidates when (direct-subclassp class (symbol-value c)) do (return c) finally (return nil)))

;; Function to select the next class on the precedence list from the list of candidates
(defun select-candidate (candidates acc)
  (cond
   ;; If there are no candidates, then there is a cycle and a contradiction on the precedence list
   ;; Therefore, an error is signaled
   ((eql (length candidates) 0) (error "There is a cycle in the precedence list"))
   ;; If the list of candidates only has one class, that is the selected class
   ((eql (length candidates) 1) (car candidates))
   ;; If the list of candidates has more than one class
   ;; For each class on the already calculated partial precedence list, check if it is a subclass of any of the candidates
   ;; The first candidate that fulfils this condition, is the next selected class for the precedence list
   ;; Considering that the classes are added to the precedence list from right to left (using a cons function)
   ;; The first class that is a subclass of any of the candidates, is located at the far right end of the partial precedence list (most recent added class)
   ;; This follows the standard algorithm
   ;; Note: For each class in the partial precedence list, there is either zero or only one candidate that is a superclass.
   ;; This is the case because a class can be a candidate if and only if is not preceded by any class in the set R
   ;; And the other superclasses would be preceded by the class that is on the beginning of the superclasses list
   (t (let ((superclass (get-superclass-of-subclass-in-list (symbol-value (car acc)) candidates)))
        (or superclass (select-candidate candidates (cdr acc)))))))

;; Function to calculate the precedence list, using an accumulator to store the result
(defun precedence-list-accumulator (s r acc)
  (if (eql (length s) 0)
      ;; If there are no more elements on the set S, the process is finished and the accumulator is returned
      acc
      ;; Otherwise, the next class is selected based on the candidates that are not preceded by any class on the set R
      (let ((elem (select-candidate (classes-not-preceded s r) acc)))
        ;; Then, the process is repeated without that class on the set S and without the couples where that class appears on the set R
        ;; And, with the selected class added to the accumulator using a cons operation
        (precedence-list-accumulator (remove-from-set elem s) (remove-apperance-of-class elem r) (cons elem acc)))))

;; Function to calculate the class precedence list
;; This function generates the sets S and R for the class, and uses the function precedence-list-accumulator with an empty accumulator
;; Finally, it reverses the result, so it goes from the most specific to the least specific
(defun class-precedence-list (class)
  (reverse (cons 't (precedence-list-accumulator (generate-class-symbol-set class) (generate-class-symbol-relation-set class) '()))))