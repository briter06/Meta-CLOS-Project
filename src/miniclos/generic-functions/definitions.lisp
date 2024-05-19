(in-package :closless)
(load "src/utils/lists.lisp")
(load "src/miniclos/precedence-list/precedence-list.lisp")

(define-condition generic-function-error (error)
    ((message :initarg :message :reader message)))

(defstruct generic-function
  (methods '())
  (num-args 0))

(defstruct method
  (specializers '())
  (function (error "No method function provided.")))

(defun find-method (accessor gf specializers)
  (loop for method in (funcall accessor gf)
          when (equal specializers (method-specializers method))
          return method))

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
    (remove method (generic-function-methods gf))))

(defun add-method (gf method)
  (let ((old-method (find-method #'generic-function-methods gf (method-specializers method))))
    (when old-method
          (remove-method gf old-method)))
  (push method (generic-function-methods gf)))

(defun compute-applicable-methods (methods arguments)
  (loop for method in methods
          when (let ((specializers (method-specializers method)))
                 (and (eql (length specializers) (length arguments))
                      (every (lambda (s) (instancep (cadr s) (car s))) (zip specializers arguments))))
        collect method))

(defun is-more-specific? (main-class class1 class2)
  (let ((precedence-list (class-precedence-list main-class)))
    (< (position (class-name-symbol class1) precedence-list) (position (class-name-symbol class2) precedence-list))))

(defun is-more-specific-list? (arguments specializers1 specializers2)
  (some (lambda (tuple) (is-more-specific? (object-class (first tuple)) (second tuple) (third tuple))) (zip3 arguments specializers1 specializers2)))

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
  (let* ((applicable-methods (compute-applicable-methods (generic-function-methods gf) arguments))
         (most-specific-method (select-most-specific-method arguments applicable-methods)))
    (funcall (method-function most-specific-method)
      arguments
      (remove most-specific-method applicable-methods))))