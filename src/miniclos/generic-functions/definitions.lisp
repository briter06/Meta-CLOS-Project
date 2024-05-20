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
  (argument-precedence-order '())
  (cache (make-hash-table)))

(defun arguments-length (gf)
  (length (generic-function-argument-precedence-order gf)))

(defun get-from-cache (gf arguments)
  (gethash (sxhash arguments) (generic-function-cache gf)))

(defun add-to-cache (gf arguments result)
  (setf (gethash (sxhash arguments) (generic-function-cache gf)) result))

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
                      (every (lambda (tuple)
                               (let ((specializer (car tuple))
                                     (argument (cadr tuple)))
                                 (if (listp specializer)
                                     (cond
                                      ((eql (car specializer) 'eql) (eql (symbol-value (cadr specializer)) argument))
                                      (t (error "Invalid specializer")))
                                     (instancep argument (symbol-value specializer))))) (zip specializers arguments))))
        collect method))

(defun is-more-specific? (main-class specializer1 specializer2)
  (cond
   ((listp specializer1) t)
   ((listp specializer2) nil)
   (t (let ((precedence-list (class-precedence-list main-class)))
        (< (position specializer1 precedence-list) (position specializer2 precedence-list))))))

(defun is-more-specific-list? (precedence-order arguments specializers1 specializers2)
  (if precedence-order
      (let ((main-class (object-class (nth (car precedence-order) arguments)))
            (specializer1 (nth (car precedence-order) specializers1))
            (specializer2 (nth (car precedence-order) specializers2)))
        (cond
         ((eql specializer1 specializer2) (is-more-specific-list? (cdr precedence-order) arguments specializers1 specializers2))
         (t (is-more-specific? main-class specializer1 specializer2))))
      nil))

(defun select-most-specific-method (precedence-order arguments methods)
  (loop with candidate = (first methods)
        for method in (rest methods)
          when (is-more-specific-list?
                precedence-order
                arguments
                (method-specializers method)
                (method-specializers candidate))
        do (setq candidate method)
        finally (return candidate)))

(defun call-generic-function-helper (precedence-order methods arguments)
  (let* ((applicable-methods (compute-applicable-methods methods arguments))
         (most-specific-method (select-most-specific-method precedence-order arguments applicable-methods)))
    (funcall (method-function most-specific-method)
      arguments
      (remove most-specific-method applicable-methods))))

(defun call-main-generic-function (gf arguments)
  (when (generic-function-before-methods gf)
        (call-generic-function-helper (generic-function-argument-precedence-order gf) (generic-function-before-methods gf) arguments))
  (let ((result (call-generic-function-helper (generic-function-argument-precedence-order gf) (generic-function-methods gf) arguments)))
    (when (generic-function-after-methods gf)
          (call-generic-function-helper (generic-function-argument-precedence-order gf) (generic-function-after-methods gf) arguments))
    result))

(defun call-generic-function (gf &rest arguments)
  (if (generic-function-around-methods gf)
      (call-generic-function-helper (generic-function-argument-precedence-order gf) (generic-function-around-methods gf) arguments)
      (call-main-generic-function gf arguments)))