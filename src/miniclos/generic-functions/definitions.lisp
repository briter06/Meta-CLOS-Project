(in-package :closless)
(load "src/utils/lists.lisp")
(load "src/miniclos/precedence-list/precedence-list.lisp")

(defstruct generic-function
  (methods '()))

(defstruct method
  (specializers '())
  (function (error "No method function provided.")))

(defun find-method (gf specializers)
  (loop for method in (generic-function-methods gf)
          when (equal specializers (method-specializers method))
          return method))

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
    (remove method (generic-function-methods gf))))

(defun add-method (gf method)
  (let ((old-method (find-method gf (method-specializers method))))
    (when old-method
          (remove-method gf old-method)))
  (push method (generic-function-methods gf)))

(defun compute-applicable-methods (gf arguments)
  (loop for method in (generic-function-methods gf)
          ; when (instancep receiver (method-specializer method))
          when (let ((specializers (method-specializers method)))
                 (and (eql (length specializers) (length arguments))
                      (every (lambda (s) (instancep (cadr s) (car s))) (zip specializers arguments))))
        collect method))

(defun is-more-specific? (main-class class1 class2)
  (let ((precedence-list (class-precedence-list main-class)))
    (< (position (class-name-symbol class1) precedence-list) (position (class-name-symbol class2) precedence-list))))

(defun is-more-specific-list? (arguments specializers1 specializers2)
  (some (lambda (tuple) (is-more-specific? (object-class (first tuple)) (second tuple) (third tuple))) (zip3 arguments specializers1 specializers2)))

#|
(defclass food () ())
(defclass spice (food) ())
(defclass fruit (food) ())
(defclass cinnamon (spice) ())
(defclass apple (fruit) ())
(defclass pie (apple cinnamon) ())

(defvar pie-instance (make-instance 'pie))
(is-more-specific-list? (list pie-instance) (list apple) (list food))
|#

(defun select-most-specific-method (arguments methods)
  (loop with candidate = (first methods)
        for method in (rest methods)
          when (is-more-specific-list?
                arguments
                (method-specializers method)
                (method-specializers candidate))
        do (setq candidate method)
        finally (return candidate)))

(defun call-generic-function (gf &rest arguments)
  (let* ((applicable-methods (compute-applicable-methods gf arguments))
         (most-specific-method (select-most-specific-method arguments applicable-methods)))
    (funcall (method-function most-specific-method)
      arguments
      (remove most-specific-method applicable-methods))))