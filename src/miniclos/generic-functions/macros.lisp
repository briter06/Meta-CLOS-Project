(in-package :closless)
(load "src/utils/lists.lisp")
(load "src/utils/with-gensyms.lisp")

(defmacro defgeneric (name)
  `(defvar ,name (make-generic-function)))

(defmacro gen-let (names values body)
  `(let ,(zip names values) ,body))

(defun extract-specializers-variables (arguments)
  (reduce (lambda (acc tuple)
            (if (listp tuple)
                `(,(cons (car tuple) (car acc)) ,(cons (cadr tuple) (cadr acc)))
                `(,(cons tuple (car acc)) ,(cons '*object* (cadr acc)))))
      (reverse arguments) :initial-value '(() ())))

(defmacro defmethod (name arguments &rest body)
  (let ((args-mapper (extract-specializers-variables arguments)))
    (with-gensyms (lambda-arguments lambda-next-methods)
                  `(add-method ,name
                               (make-method
                                :specializers ,(cons 'list (cadr args-mapper))
                                :function (lambda (,lambda-arguments ,lambda-next-methods)
                                            (declare (ignore ,lambda-next-methods))
                                            (let ,(loop for name in (car args-mapper) for index from 0 collect `(,name (nth ,index ,lambda-arguments)))
                                              (labels ((call-next-method () (print "Called"))) ,(cons 'progn body)))))))))


; (macroexpand '(defmethod display ((person-obj person)) (print (slot-value person-obj 'name))))