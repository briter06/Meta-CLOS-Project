(in-package :closless)
(load "src/utils/sets.lisp")

;; Predecence list

(define-condition predecence-list-error (error)
    ((message :initarg :message :reader message)))

(defun generate-class-symbol-set (class)
  (adjoin (class-name-symbol class) (mapcar #'class-name-symbol (class-all-superclasses class))))

(defun get-couples (elems)
  (cond
   ((< (length elems) 2) ())
   (t (adjoin (list (car elems) (cadr elems)) (get-couples (cdr elems))))))

(defun generate-class-symbol-relation-set (class)
  (let ((symbols (mapcar #'class-name-symbol (class-direct-superclasses class))))
    (cond
     ((eql class *object*) '((*object* t)))
     (t (reduce (lambda (acc x)
                  (concat-sets (generate-class-symbol-relation-set (symbol-value x)) acc :test #'equal))
            symbols
          :initial-value (adjoin (list (class-name-symbol class) (car symbols)) (get-couples symbols) :test #'equal))))))

(defun is-preceded (class-symbol r)
  (loop for tuple in r when (eql (cadr tuple) class-symbol) do (return t) finally (return nil)))

(defun classes-not-preceded (s r)
  (remove-if (lambda (symbol) (is-preceded symbol r)) s))

(defun remove-apperance-of-class (symbol r)
  (remove-if (lambda (tuple) (or (eql (car tuple) symbol) (eql (cadr tuple) symbol))) r))

(defun get-superclass-of-subclass-in-list (class candidates)
  ;; TODO -> DIRECT SUBCLASS
  (loop for c in candidates when (subclassp class (symbol-value c)) do (return c) finally (return nil)))

(defun detect-superclass (candidates acc)
  (cond
   ((eql (length candidates) 1) (car candidates))
   ((eql (length acc) 0) (error 'predecence-list-error :message "There is a cycle in the precedence list"))
   (t (let ((superclass (get-superclass-of-subclass-in-list (symbol-value (car acc)) candidates)))
        (or superclass (detect-superclass candidates (cdr acc)))))))

(defun precedence-list-accumulator (s r acc)
  (if (eql (length s) 0)
      acc
      (let ((candidates (classes-not-preceded s r)))
        (if
         (eql (length candidates) 0) (error 'predecence-list-error :message "There is a cycle in the precedence list")
         (let ((elem (detect-superclass candidates acc)))
           (precedence-list-accumulator (remove-from-set elem s) (remove-apperance-of-class elem r) (cons elem acc)))))))

(defun class-precedence-list (class)
  (reverse (cons 't (precedence-list-accumulator (generate-class-symbol-set class) (generate-class-symbol-relation-set class) '()))))