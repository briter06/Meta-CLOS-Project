(in-package :closless)
(load "src/miniclos/classes/definitions.lisp")
(load "src/utils/lists.lisp")
(load "src/utils/with-gensyms.lisp")

;; Macro to create a generic function
(defmacro defgeneric (name &rest all-arguments)
  (append
    `(progn
      ;; Create the generic function
      (defvar ,name (make-generic-function
                     :argument-precedence-order
                     ;; Get the positions of the elements of the second list based on the first list
                     ',(get-positions
                        ;; The base is the list of arguments
                        (car all-arguments)
                        (or
                         ;; If the argument with key :argument-precedence-order is provided, it is used as the second list
                         (cdr (assoc :argument-precedence-order (cdr all-arguments)))
                         ;; If not, the same arguments are use, so the result is always a sequence from 0 until the number of arguments
                         (car all-arguments))))))
    (cond
     ;; If there is an argument with the key :cached
     ;; An around method is automatically added to cache the result
     ((assoc :cached (cdr all-arguments))
       (let ((args-cache (cons 'list (car all-arguments))))
         ;; Add around method with the defmethod macro
         `((defmethod ,name :around ,(mapcar (lambda (x) `(,x *object*)) (car all-arguments))
             ;; If the result is cached, it's directly returned
             (or (get-from-cache ,name ,args-cache)
                 ;; If not, the method is called and cached
                 (let ((result (call-next-method)))
                   (add-to-cache ,name ,args-cache result)
                   result))))))
     (t '()))
    ;; A function is created, which redirects the call to the method call-generic-function
    ;; This is used to call the function like in CLOS
    `((defun ,name (&rest arguments) (apply #'call-generic-function (cons ,name arguments))))))

;; Function to transform an specializer symbol
(defun transform-specializer (specializer)
  (if (listp specializer)
      ;; If it is a list (EQL specializer), not mangle it
      specializer
      ;; If not, mangle the name of the class
      (mangle-class-name specializer)))

;; Function to extract the specializers and the objects from the arguments
(defun extract-specializers-variables (arguments)
  (reduce (lambda (acc tuple)
            (let ((objects (car acc))
                  (specializers (cadr acc)))
              (if (listp tuple)
                  ;; If the argument is a list, it means that the developer defined the variable name and the type
                  ;; Based on this, the variable name and the specializer are separated, and the later is transformed
                  `(,(cons (car tuple) objects) ,(cons (transform-specializer (cadr tuple)) specializers))
                  ;; If it's not a list, it means that the developer only defined the variable name
                  ;; In this cases, the specializer class is set to the standard object *object*
                  `(,(cons tuple objects) ,(cons '*object* specializers)))))
      (reverse arguments) :initial-value '(() ())))

;; Function to call the next most specific method, based on the next-methods list
(defun next-method-helper (gf arguments next-methods)
  ;; Use gensym so the name of the variable is unique and it doesn't create a conflict with the variables used by the developer
  (with-gensyms (next-most-specific-method)
                `(let ((,next-most-specific-method (select-most-specific-method (generic-function-argument-precedence-order ,gf) ,arguments ,next-methods)))
                   (if ,next-most-specific-method
                       ;; If there is a next method, call it
                       (funcall (method-function ,next-most-specific-method)
                         ,arguments
                         (remove ,next-most-specific-method ,next-methods))
                       ;; If not, signal an error
                       (error "There is no available next method")))))

;; Function to call the next most specific method only if there is one, if not, call the main generic function (primary method + "before" and "after" methods)
(defun next-method-around (gf arguments next-methods)
  `(if ,next-methods
       ;; If there are more methods to run, use the regular process
       ,(next-method-helper gf arguments next-methods)
       ;; If not, call the main process of the primary methods and the "before" and "after" auxiliary methods
       (call-main-generic-function ,gf ,arguments)))

;; Function to check the number of arguments of the method is the same as the one used when defining the generic function
(defun argument-num-checker (gf arguments)
  (let ((new-num-args (length arguments))
        (num-args (arguments-length (symbol-value gf))))
    (when (not (eql new-num-args num-args))
          ;; Signal an error if the number of arguments is not equal
          (error (format nil "Invalid number of arguments: ~d" new-num-args)))))

;; Generate the let expression for binding the elements in the list of arguments with the variable names defined by the developer
(defun gen-let (variable-names lambda-arguments body)
  (append `(let ,(loop for variable-name in variable-names for index from 0 collect `(,variable-name (nth ,index ,lambda-arguments)))) body))

;; Function for creating a primary method for a generic function
;; Also used for the "around" auxiliary methods
(defun defmethod-normal (method-symbol next-method-symbol gf arguments body)
  ;; Verify the number of arguments
  (argument-num-checker gf arguments)
  ;; Separate the specializers and the arguments
  (let ((args-mapper (extract-specializers-variables arguments)))
    ;; Create the method and add it to the generic function
    `(,method-symbol ,gf
       (make-method
        ;; Define the list of specializers
        :specializers ',(cadr args-mapper)
        ;; Define the lambda function, where the arguments are unique symbols (gensym)
        :function ,(with-gensyms (lambda-arguments lambda-next-methods)
                                 ;;Define the lambda with those unique names as arguments
                                 `(lambda (,lambda-arguments ,lambda-next-methods)
                                    ;; Define a local function for calling the next method, allowing the developer to use it inside the body of the method without arguments
                                    (labels ((call-next-method () ,(funcall next-method-symbol gf lambda-arguments lambda-next-methods)))
                                      ;; Bind the variables and inject the body of the method
                                      ,(gen-let (car args-mapper) lambda-arguments body))))))))

;; Function for creating a "before" auxiliary method
(defun defmethod-before (gf arguments body)
  ;; Verify the number of arguments
  (argument-num-checker gf arguments)
  ;; Separate the specializers and the arguments
  (let ((args-mapper (extract-specializers-variables arguments)))
    ;; Create the method and add it to the generic function
    `(add-before-method ,gf
                        (make-method
                         ;; Define the list of specializers
                         :specializers ',(cadr args-mapper)
                         ;; Define the lambda function, where the arguments are unique symbols (gensym)
                         :function ,(with-gensyms (lambda-arguments lambda-next-methods)
                                                  ;;Define the lambda with those unique names as arguments
                                                  `(lambda (,lambda-arguments ,lambda-next-methods)
                                                     ;; Bind the variables and inject the body of the method
                                                     ,(gen-let (car args-mapper) lambda-arguments body)
                                                     ;; Call the next "before" method
                                                     ;; Considering that the next method is called after executing the current body
                                                     ;; It calls from the most specific to the least specific
                                                     (when ,lambda-next-methods ,(next-method-helper gf lambda-arguments lambda-next-methods))))))))

;; Function for creating an "after" auxiliary method
(defun defmethod-after (gf arguments body)
  ;; Verify the number of arguments
  (argument-num-checker gf arguments)
  ;; Separate the specializers and the arguments
  (let ((args-mapper (extract-specializers-variables arguments)))
    ;; Create the method and add it to the generic function
    `(add-after-method ,gf
                       (make-method
                        ;; Define the list of specializers
                        :specializers ',(cadr args-mapper)
                        ;; Define the lambda function, where the arguments are unique symbols (gensym)
                        :function ,(with-gensyms (lambda-arguments lambda-next-methods)
                                                 ;;Define the lambda with those unique names as arguments
                                                 `(lambda (,lambda-arguments ,lambda-next-methods)
                                                    ;; Call the next "after" method
                                                    ;; Considering that the next method is called before executing the current body
                                                    ;; It calls from the least specific to the most specific
                                                    (when ,lambda-next-methods ,(next-method-helper gf lambda-arguments lambda-next-methods))
                                                    ;; Bind the variables and inject the body of the method
                                                    ,(gen-let (car args-mapper) lambda-arguments body)))))))

;; Macro fro creating a method for a generic function
(defmacro defmethod (gf &rest all-arguments)
  ;; first -> First argument of the macro
  ;; rest -> Other arguments
  (let ((first (car all-arguments))
        (rest (cdr all-arguments)))
    (cond
     ;; If the first argument is the key :before, then use the function to add a "before" auxiliary method
     ((eql first ':before) (defmethod-before gf (car rest) (cdr rest)))
     ;; If the first argument is the key :after, then use the function to add a "after" auxiliary method
     ((eql first ':after) (defmethod-after gf (car rest) (cdr rest)))
     ;; If the first argument is the key :around, then use the function to add a primary method
     ;; But the function to call the next most specific function is next-method-around
     ((eql first ':around) (defmethod-normal 'add-around-method #'next-method-around gf (car rest) (cdr rest)))
     ;; Otherwise, it is a primary method, and the function to add a primary method is used, where the function next-method-helper is used for calling the next method
     (t (defmethod-normal 'add-method #'next-method-helper gf first rest)))))