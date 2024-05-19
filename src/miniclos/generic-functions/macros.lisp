(in-package :closless)
(load "src/utils/lists.lisp")
(load "src/utils/with-gensyms.lisp")

(defmacro defgeneric (name args)
  `(progn
      (defvar ,name (make-generic-function :num-args ,(length args)))
      (defun ,name (&rest arguments) (apply #'call-generic-function (cons ,name arguments)))))

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
  (let ((new-num-args (length arguments))
        (num-args (generic-function-num-args (symbol-value name)))
        (args-mapper (extract-specializers-variables arguments)))
    (with-gensyms (lambda-arguments lambda-next-methods)
                  (cond
                    ((not (eql new-num-args num-args))
                      `(error 'generic-function-error :message (format nil "Invalid number of arguments: ~d" ,new-num-args)))
                    (t
                      `(add-method ,name
                               (make-method
                                :specializers ,(cons 'list (cadr args-mapper))
                                :function (lambda (,lambda-arguments ,lambda-next-methods)
                                            (let ,(loop for variable-name in (car args-mapper) for index from 0 collect `(,variable-name (nth ,index ,lambda-arguments)))
                                              (labels ((call-next-method () ,(next-method-helper lambda-arguments lambda-next-methods)))
                                                ,(cons 'progn body)))))))))))