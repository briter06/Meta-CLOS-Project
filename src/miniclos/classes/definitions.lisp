(in-package :closless)
(load "src/utils/sets.lisp")

;; New defined standard object
(defvar *object*)

;; Function to mangle the name of a class
;; If the class is named 'person. The class is defined as '<person>
(defun mangle-class-name (class-symbol)
  (cond
   ((eql class-symbol '*object*) class-symbol)
   (t (intern (concatenate 'string "<" (string class-symbol) ">")))))

;; Struct for the classes with the name of the class, the list of superclasses and the list of slots
(defstruct class
  (name-symbol nil)
  (direct-superclasses (list *object*))
  (direct-slots '()))

;; Initialize the standard object as a instance of the class struct
(unless (boundp '*object*)
  (setf *object* (make-class :name-symbol '*object* :direct-superclasses nil)))

;; Get all the superclasses of a class (Not in order of specificity)
(defun class-all-superclasses (class)
  (cond
   ((eql class *object*) (list *object*))
   (t (reduce
          (lambda (acc x) (concat-sets (class-all-superclasses x) acc :test (lambda (c1 c2) (eql (class-name-symbol c1) (class-name-symbol c2)))))
          (class-direct-superclasses class) :initial-value (list class)))))

;; Check if a class is a subclass of another
(defun subclassp (class1 class2)
  (member class2 (class-all-superclasses class1)))

;; Check if a class is a direct subclass of another
(defun direct-subclassp (class1 class2)
  (member class2 (class-direct-superclasses class1)))

;; Struct for the objects (instances of a class)
(defstruct object
  (class *object*)
  (slots (make-hash-table)))

;; Function to get the value in a slot of an object
(defun slot-value (object slot-name)
  (gethash slot-name (object-slots object)))

;; Function to set the value in a slot of an object
(defun (setf slot-value) (value object slot-name)
  (setf (gethash slot-name (object-slots object))
    value))

;; Function to make an instance of a class
(defun make-instance (class)
  (make-object :class (symbol-value (mangle-class-name class))))

;; Function to find a class object based on the name (i.e. 'person)
(defun find-class (class-symbol)
  (symbol-value (mangle-class-name class-symbol)))

;; Function to check if an object is an instance of a specific class
(defun instancep (object class)
  (subclassp (object-class object) class))