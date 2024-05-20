(in-package :closless)
(load "src/miniclos/classes/definitions.lisp")
(load "src/utils/lists.lisp")
(load "src/utils/with-gensyms.lisp")

(defmacro defgeneric (name &rest all-arguments)
  (append
    `(progn (defvar ,name (make-generic-function :num-args ,(length (car all-arguments)))))
    (cond
     ((getf (cadr all-arguments) :cached)
       (let ((args-cache (cons 'list (car all-arguments))))
         `((defmethod ,name :around ,(mapcar (lambda (x) `(,x *object*)) (car all-arguments))
             (or (get-from-cache ,name ,args-cache)
                 (let ((result (call-next-method)))
                   (add-to-cache ,name ,args-cache result)
                   result))))))
     (t '()))
    `((defun ,name (&rest arguments) (apply #'call-generic-function (cons ,name arguments))))))

(defun transform-specializer (specializer)
  (if (listp specializer)
      specializer
      (mangle-class-name specializer)))

(defun extract-specializers-variables (arguments)
  (reduce (lambda (acc tuple)
            (let ((objects (car acc))
                  (specializers (cadr acc)))
              (if (listp tuple)
                  `(,(cons (car tuple) objects) ,(cons (transform-specializer (cadr tuple)) specializers))
                  `(,(cons tuple objects) ,(cons '*object* specializers)))))
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

(defun defmethod-helper (gf arguments function-body)
  (let ((new-num-args (length arguments))
        (num-args (generic-function-num-args (symbol-value gf))))
    (cond
     ((not (eql new-num-args num-args))
       `(error 'generic-function-error :message (format nil "Invalid number of arguments: ~d" ,new-num-args)))
     (t function-body))))

(defun gen-let (variable-names lambda-arguments body)
  (append `(let ,(loop for variable-name in variable-names for index from 0 collect `(,variable-name (nth ,index ,lambda-arguments)))) body))

(defun defmethod-normal (method-symbol next-method-symbol gf arguments body)
  (defmethod-helper gf arguments
                    (let ((args-mapper (extract-specializers-variables arguments)))
                      `(,method-symbol ,gf
                         (make-method
                          :specializers ',(cadr args-mapper)
                          :function ,(with-gensyms (lambda-arguments lambda-next-methods)
                                                   `(lambda (,lambda-arguments ,lambda-next-methods)
                                                      (labels ((call-next-method () ,(funcall next-method-symbol gf lambda-arguments lambda-next-methods)))
                                                        ,(gen-let (car args-mapper) lambda-arguments body)))))))))

(defun defmethod-before (gf arguments body)
  (defmethod-helper gf arguments
                    (let ((args-mapper (extract-specializers-variables arguments)))
                      `(add-before-method ,gf
                                          (make-method
                                           :specializers ',(cadr args-mapper)
                                           :function ,(with-gensyms (lambda-arguments lambda-next-methods)
                                                                    `(lambda (,lambda-arguments ,lambda-next-methods)
                                                                       ,(gen-let (car args-mapper) lambda-arguments body)
                                                                       (when ,lambda-next-methods ,(next-method-helper gf lambda-arguments lambda-next-methods)))))))))

(defun defmethod-after (gf arguments body)
  (defmethod-helper gf arguments
                    (let ((args-mapper (extract-specializers-variables arguments)))
                      `(add-after-method ,gf
                                         (make-method
                                          :specializers ',(cadr args-mapper)
                                          :function ,(with-gensyms (lambda-arguments lambda-next-methods)
                                                                   `(lambda (,lambda-arguments ,lambda-next-methods)
                                                                      (when ,lambda-next-methods ,(next-method-helper gf lambda-arguments lambda-next-methods))
                                                                      ,(gen-let (car args-mapper) lambda-arguments body))))))))

(defmacro defmethod (gf &rest all-arguments)
  (let ((first (car all-arguments))
        (rest (cdr all-arguments)))
    (cond
     ((eql first ':before) (defmethod-before gf (car rest) (cdr rest)))
     ((eql first ':after) (defmethod-after gf (car rest) (cdr rest)))
     ((eql first ':around) (defmethod-normal 'add-around-method #'next-method-around gf (car rest) (cdr rest)))
     (t (defmethod-normal 'add-method #'next-method-helper gf first rest)))))