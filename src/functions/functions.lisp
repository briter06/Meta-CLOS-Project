(in-package :closless)

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