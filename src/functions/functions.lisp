(in-package :closless)
(load "src/utils/sets.lisp")

(defun generate-class-symbol-set (class)
  (adjoin (class-name-symbol class) (mapcar #'class-name-symbol (class-all-superclasses class))))

(defun equal-setp (set1 set2) (null (set-difference set1 set2)))

(defun get-couples (elems)
  (cond
   ((< (length elems) 2) ())
   (t (adjoin (list (car elems) (cadr elems)) (get-couples (cdr elems))))))

(defun generate-class-symbol-relation-set (class)
  (let ((symbols (mapcar #'class-name-symbol (class-direct-superclasses class))))
    (cond
     ((eql class *object*) '((*object* t)))
     (t (reduce (lambda (acc x)
                  (concat-sets (generate-class-symbol-relation-set (find-class x)) acc :test #'equal-setp))
            symbols
          :initial-value (adjoin (list (class-name-symbol class) (car symbols)) (get-couples symbols) :test #'equal-setp))))))

(defun is-preceded (class-symbol r)
  (loop for tuple in r when (eql (cadr tuple) class-symbol) do (return t) finally (return nil)))

(defun classes-not-preceded (s r)
  (remove-if (lambda (symbol) (is-preceded symbol r)) s))

(defun remove-apperance-of-class (symbol r)
  (remove-if (lambda (tuple) (or (eql (car tuple) symbol) (eql (cadr tuple) symbol))) r))

(defun get-superclass-of-subclass-in-list (class candidates)
  (loop for c in candidates when (subclassp class (find-class c)) do (return c) finally (return nil)))

(defun detect-superclass (candidates acc)
  (cond
   ((eql (length candidates) 1) (car candidates))
   ((eql (length acc) 0) (error "TODO error"))
   (t (let ((superclass (get-superclass-of-subclass-in-list (find-class (car acc)) candidates)))
        (or superclass (detect-superclass candidates (cdr acc)))))))

(defun precedence-list-accumulator (s r acc)
  (let ((candidates (classes-not-preceded s r)))
    (cond
     ((eql (length candidates) 0) acc)
     (t
       (let ((elem (detect-superclass candidates acc)))
         (precedence-list-accumulator (remove-from-set elem s) (remove-apperance-of-class elem r) (cons elem acc)))))))

(defun class-precedence-list (class)
  (reverse (cons 't (precedence-list-accumulator (generate-class-symbol-set class) (generate-class-symbol-relation-set class) '()))))

; (class-precedence-list (find-class 'pie))