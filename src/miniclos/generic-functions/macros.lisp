(in-package :closless)
(load "src/utils/lists.lisp")
(load "src/utils/with-gensyms.lisp")

(define-condition generic-function-error (error)
    ((message :initarg :message :reader message)))

(defmacro defgeneric (name)
  `(defvar ,name (make-generic-function)))

(defun extract-specializers-variables (arguments)
  (reduce (lambda (acc tuple)
            (if (listp tuple)
                `(,(cons (car tuple) (car acc)) ,(cons (cadr tuple) (cadr acc)))
                `(,(cons tuple (car acc)) ,(cons '*object* (cadr acc)))))
      (reverse arguments) :initial-value '(() ())))

(defun next-method-helper (arguments next-methods)
  (with-gensyms (next-most-specific-method)
    `(let ((,next-most-specific-method (select-most-specific-method ,arguments ,next-methods)))
        (if ,next-most-specific-method
          (funcall (method-function ,next-most-specific-method)
            ,arguments
            (remove ,next-most-specific-method ,next-methods))
          (error 'generic-function-error :message "There is no available next method")))))

(defmacro defmethod (name arguments &rest body)
  (let ((args-mapper (extract-specializers-variables arguments)))
    (with-gensyms (lambda-arguments lambda-next-methods)
                  `(add-method ,name
                               (make-method
                                :specializers ,(cons 'list (cadr args-mapper))
                                :function (lambda (,lambda-arguments ,lambda-next-methods)
                                            (let ,(loop for name in (car args-mapper) for index from 0 collect `(,name (nth ,index ,lambda-arguments)))
                                              (labels ((call-next-method () ,(next-method-helper lambda-arguments lambda-next-methods)))
                                                ,(cons 'progn body)))))))))


; (macroexpand '(defmethod display ((person-obj person)) (print (slot-value person-obj 'name))))