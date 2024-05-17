(in-package :closless)

(defun concat-sets (set1 set2)
  (reduce (lambda (acc x) (adjoin x acc)) set1 :initial-value set2))

(defun make-instance (class)
  (make-object :class (symbol-value class)))

(defun find-class (class-symbol)
  (symbol-value class-symbol))

(defun generate-class-set (class)
  (cond
   ((eql class *object*) ())
   (t (reduce (lambda (acc x) (concat-sets (generate-class-set (find-class x)) (adjoin x acc))) (class-direct-superclass class) :initial-value ()))))