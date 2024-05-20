(in-package :closless)
(load "src/miniclos/classes/definitions.lisp")
(load "src/utils/lists.lisp")
(load "src/utils/with-gensyms.lisp")

(defmacro defgeneric (name args)
  `(progn
    (defvar ,name (make-generic-function :num-args ,(length args)))
    (defun ,name (&rest arguments) (apply #'call-generic-function (cons ,name arguments)))))

(defun extract-specializers-variables (arguments)
  (reduce (lambda (acc tuple)
            (if (listp tuple)
                `(,(cons (car tuple) (car acc)) ,(cons (mangle-class-name (cadr tuple)) (cadr acc)))
                `(,(cons tuple (car acc)) ,(cons '*object* (cadr acc)))))
      (reverse arguments) :initial-value '(() ())))

(defun next-method-helper (gf arguments next-methods)
  (declare (ignore gf))
  (with-gensyms (next-most-specific-method)
                `(let ((,next-most-specific-method (select-most-specific-method ,arguments ,next-methods)))
                   (if ,next-most-specific-method
                       (funcall (method-function ,next-most-specific-method)
                         ,arguments
                         (remove ,next-most-specific-method ,next-methods))
                       (error 'generic-function-error :message "There is no available next method")))))

(defun next-method-around (gf arguments next-methods)
  `(if ,next-methods
      ,(next-method-helper gf arguments next-methods)
      (call-main-generic-function ,gf ,arguments)))

(defun defmethod-helper (name arguments function-body)
  (let ((new-num-args (length arguments))
        (num-args (generic-function-num-args (symbol-value name))))
    (cond
     ((not (eql new-num-args num-args))
       `(error 'generic-function-error :message (format nil "Invalid number of arguments: ~d" ,new-num-args)))
     (t function-body))))

(defun gen-let (variable-names lambda-arguments body)
  (append `(let ,(loop for variable-name in variable-names for index from 0 collect `(,variable-name (nth ,index ,lambda-arguments)))) body))

(defun defmethod-normal (method-symbol next-method-symbol name arguments body)
  (defmethod-helper name arguments
                    (let ((args-mapper (extract-specializers-variables arguments)))
                      `(,method-symbol ,name
                         (make-method
                          :specializers ,(cons 'list (cadr args-mapper))
                          :function ,(with-gensyms (lambda-arguments lambda-next-methods)
                                                   `(lambda (,lambda-arguments ,lambda-next-methods)
                                                      (labels ((call-next-method () ,(funcall next-method-symbol name lambda-arguments lambda-next-methods)))
                                                        ,(gen-let (car args-mapper) lambda-arguments body)))))))))

(defun defmethod-before (name arguments body)
  (defmethod-helper name arguments
                    (let ((args-mapper (extract-specializers-variables arguments)))
                      `(add-before-method ,name
                                          (make-method
                                           :specializers ,(cons 'list (cadr args-mapper))
                                           :function ,(with-gensyms (lambda-arguments lambda-next-methods)
                                                                    `(lambda (,lambda-arguments ,lambda-next-methods)
                                                                       ,(gen-let (car args-mapper) lambda-arguments body)
                                                                       (when ,lambda-next-methods ,(next-method-helper name lambda-arguments lambda-next-methods)))))))))

(defun defmethod-after (name arguments body)
  (defmethod-helper name arguments
                    (let ((args-mapper (extract-specializers-variables arguments)))
                      `(add-after-method ,name
                                         (make-method
                                          :specializers ,(cons 'list (cadr args-mapper))
                                          :function ,(with-gensyms (lambda-arguments lambda-next-methods)
                                                                   `(lambda (,lambda-arguments ,lambda-next-methods)
                                                                      (when ,lambda-next-methods ,(next-method-helper name lambda-arguments lambda-next-methods))
                                                                      ,(gen-let (car args-mapper) lambda-arguments body))))))))

(defmacro defmethod (name &rest all-arguments)
  (let ((first (car all-arguments))
        (rest (cdr all-arguments)))
    (cond
     ((eql first ':before) (defmethod-before name (car rest) (cdr rest)))
     ((eql first ':after) (defmethod-after name (car rest) (cdr rest)))
     ((eql first ':around) (defmethod-normal 'add-around-method #'next-method-around name (car rest) (cdr rest)))
     (t (defmethod-normal 'add-method #'next-method-helper name first rest)))))