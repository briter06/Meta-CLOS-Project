(in-package :closless)

(defun concat-sets (set1 set2 &key test)
  (reduce (lambda (acc x) (adjoin x acc :test (or test #'equal))) set1 :initial-value set2))

(defun make-instance (class)
  (make-object :class (symbol-value class)))

(defun find-class (class-symbol)
  (symbol-value class-symbol))

(defun generate-class-set (class-symbol)
  (let ((class (find-class class-symbol)))
    (cond
     ((eql class *object*) ())
     (t (reduce
            (lambda (acc x) (concat-sets (generate-class-set x) (adjoin x acc)))
            (class-direct-superclass class) :initial-value (list class-symbol))))))

(defun equal-setp (set1 set2) (null (set-difference set1 set2)))

(defun get-couples (elems)
  (cond
   ((< (length elems) 2) ())
   (t (adjoin (list (car elems) (cadr elems)) (get-couples (cdr elems))))))

(defun generate-relation-set (class-symbol)
  (let ((class (find-class class-symbol))
        (classes (class-direct-superclass (find-class class-symbol))))
    (cond
     ((eql class *object*) ())
     (t (reduce (lambda (acc x)
                  (concat-sets (generate-relation-set x) acc :test #'equal-setp))
            classes
          :initial-value (adjoin (list class-symbol (car classes)) (get-couples classes) :test #'equal-setp))))))