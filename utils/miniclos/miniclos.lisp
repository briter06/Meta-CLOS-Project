(in-package :closless)

(defvar *object*)

(defstruct class
  (direct-superclass *object*)
  (direct-slots '()))

(unless (boundp '*object*)
  (setf *object* (make-class :direct-superclass nil)))

(defun class-all-superclasses (class)
  (loop for c = class then (class-direct-superclass c)
        while c
        collect c))

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

(defun instancep (object class)
  (subclassp (object-class object) class))

(defstruct generic-function
  (methods '()))

(defstruct method
  (specializer *object*)
  (function (error "No method function provided.")))

(defun find-method (gf specializer)
  (loop for method in (generic-function-methods gf)
        when (eql specializer (method-specializer method))
        return method))

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
        (remove method (generic-function-methods gf))))

(defun add-method (gf method)
  (let ((old-method (find-method gf (method-specializer method))))
    (when old-method
      (remove-method gf old-method)))
  (push method (generic-function-methods gf)))

(defun compute-applicable-methods (gf receiver)
  (loop for method in (generic-function-methods gf)
        when (instancep receiver (method-specializer method))
        collect method))

(defun select-most-specific-method (methods)
  (loop with candidate = (first methods)
        for method in (rest methods)
        when (subclassp (method-specializer method)
                        (method-specializer candidate))
        do (setq candidate method)
        finally (return candidate)))

(defun call-generic-function (gf receiver &rest args)
  (let* ((applicable-methods (compute-applicable-methods gf receiver))
         (most-specific-method (select-most-specific-method applicable-methods)))
    (funcall (method-function most-specific-method)
             receiver args
             (remove most-specific-method applicable-methods))))
