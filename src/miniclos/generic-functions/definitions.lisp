(in-package :closless)
(load "src/utils/lists.lisp")
(load "src/miniclos/precedence-list/precedence-list.lisp")

(define-condition generic-function-error (error)
    ((message :initarg :message :reader message)))

(defstruct generic-function
  (methods '())
  (before-methods '())
  (after-methods '())
  (around-methods '())
  (num-args 0))

(defstruct method
  (specializers '())
  (function (error "No method function provided.")))

(defun find-method (method-list specializers)
  (loop for method in method-list
          when (equal specializers (method-specializers method))
          return method))

(defmacro remove-method (method-list method)
  `(setf ,method-list
     (remove ,method ,method-list)))

(defmacro add-method-helper (method-list method)
  `(progn
    (let ((old-method (find-method ,method-list (method-specializers ,method))))
      (when old-method
            (remove-method ,method-list old-method)))
    (push ,method ,method-list)))

(defun add-method (gf method)
  (add-method-helper (generic-function-methods gf) method))

(defun add-before-method (gf method)
  (add-method-helper (generic-function-before-methods gf) method))

(defun add-after-method (gf method)
  (add-method-helper (generic-function-after-methods gf) method))

(defun add-around-method (gf method)
  (add-method-helper (generic-function-around-methods gf) method))

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
  (let ((main-class (object-class (car arguments)))
        (class1 (car specializers1))
        (class2 (car specializers2)))
    (cond
     ((eql class1 class2) (is-more-specific-list? (cdr arguments) (cdr specializers1) (cdr specializers2)))
     (t (is-more-specific? main-class class1 class2)))))

(defun select-most-specific-method (arguments methods)
  (loop with candidate = (first methods)
        for method in (rest methods)
          when (is-more-specific-list?
                arguments
                (method-specializers method)
                (method-specializers candidate))
        do (setq candidate method)
        finally (return candidate)))

(defun call-generic-function-helper (methods arguments)
  (let* ((applicable-methods (compute-applicable-methods methods arguments))
         (most-specific-method (select-most-specific-method arguments applicable-methods)))
    (funcall (method-function most-specific-method)
      arguments
      (remove most-specific-method applicable-methods))))

(defun call-main-generic-function (gf arguments)
  (when (generic-function-before-methods gf)
        (call-generic-function-helper (generic-function-before-methods gf) arguments))
  (let ((result (call-generic-function-helper (generic-function-methods gf) arguments)))
    (when (generic-function-after-methods gf)
          (call-generic-function-helper (generic-function-after-methods gf) arguments))
    result))

(defun call-generic-function (gf &rest arguments)
  (if (generic-function-around-methods gf)
      (call-generic-function-helper (generic-function-around-methods gf) arguments)
      (call-main-generic-function gf arguments)))