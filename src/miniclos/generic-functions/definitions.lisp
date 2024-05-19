(in-package :closless)

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