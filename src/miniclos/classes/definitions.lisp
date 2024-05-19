(in-package :closless)
(load "src/utils/sets.lisp")

(defvar *object*)

(defun mangle-class-name (class-symbol)
  (cond
    ((eql class-symbol '*object*) class-symbol)
    (t (intern (concatenate 'string "<" (string class-symbol) ">")))))

(defstruct class
  (name-symbol nil)
  (direct-superclasses (list *object*))
  (direct-slots '()))

(unless (boundp '*object*)
  (setf *object* (make-class :name-symbol '*object* :direct-superclasses nil)))

(defun class-all-superclasses (class)
  (cond
   ((eql class *object*) (list *object*))
   (t (reduce
          (lambda (acc x) (concat-sets (class-all-superclasses x) acc :test (lambda (c1 c2) (eql (class-name-symbol c1) (class-name-symbol c2)))))
          (class-direct-superclasses class) :initial-value (list class)))))

(defun subclassp (class1 class2)
  (member class2 (class-all-superclasses class1)))

(defstruct object
  (class *object*)
  (slots (make-hash-table)))

(defun slot-value (object slot-name)
  (gethash slot-name (object-slots object)))

(defun (setf slot-value) (value object slot-name)
  (setf (gethash slot-name (object-slots object))
    value))

(defun make-instance (class)
  (make-object :class (symbol-value (mangle-class-name class))))

(defun find-class (class-symbol)
  (symbol-value (mangle-class-name class-symbol)))

(defun instancep (object class)
  (subclassp (object-class object) class))