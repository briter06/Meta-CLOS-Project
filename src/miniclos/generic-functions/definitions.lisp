(in-package :closless)
(load "src/utils/lists.lisp")
(load "src/miniclos/precedence-list/precedence-list.lisp")

;; Struct for the generic function
(defstruct generic-function
  ;; List of the primmethods
  (methods '())
  ;; List for the "before" auxiliary methods
  (before-methods '())
  ;; List for the "after" auxiliary methods
  (after-methods '())
  ;; List for the "around" auxiliary methods
  (around-methods '())
  ;; List with the indexes for the argument precedence order
  (argument-precedence-order '())
  ;; Hash table with the cached results
  (cache (make-hash-table)))

;; Function to get the number of argument of the generic function
(defun arguments-length (gf)
  (length (generic-function-argument-precedence-order gf)))

;; Function to get the result from cache based on the arguments
(defun get-from-cache (gf arguments)
  (gethash (sxhash arguments) (generic-function-cache gf)))

;; Function to save into cache a result based on the arguments
(defun add-to-cache (gf arguments result)
  (setf (gethash (sxhash arguments) (generic-function-cache gf)) result))

;; Struct for the methods of a generic function
(defstruct method
  ;; List of specializers
  (specializers '())
  ;; Lambda function
  (function (error "No method function provided.")))

;; Find a method in a method list based on the specializers
(defun find-method (method-list specializers)
  (loop for method in method-list
          when (equal specializers (method-specializers method))
          return method))

;; Remove a method from a method list
(defmacro remove-method (method-list method)
  `(setf ,method-list
     (remove ,method ,method-list)))

;; Add a method into a method list
(defmacro add-method-helper (method-list method)
  `(progn
    (let ((old-method (find-method ,method-list (method-specializers ,method))))
      ;; In case the method already exists (same specializers), it's replaced with the new one
      (when old-method
            (remove-method ,method-list old-method)))
    (push ,method ,method-list)))

;; Function to add a primary method to the generic function
(defun add-method (gf method)
  (add-method-helper (generic-function-methods gf) method))

;; Function to add a "before" auxiliary method to the generic function
(defun add-before-method (gf method)
  (add-method-helper (generic-function-before-methods gf) method))

;; Function to add an "after" auxiliary method to the generic function
(defun add-after-method (gf method)
  (add-method-helper (generic-function-after-methods gf) method))

;; Function to add an "around" auxiliary method to the generic function
(defun add-around-method (gf method)
  (add-method-helper (generic-function-around-methods gf) method))

;; Compute the methods that are applicable based on the arguments of the function
(defun compute-applicable-methods (methods arguments)
  (loop for method in methods
          ;; For each method in the method list, every argument is compared with its corresponding specializer
          when (let ((specializers (method-specializers method)))
                 (and (eql (length specializers) (length arguments))
                      (every (lambda (tuple)
                               ;; The first element of the tuple is the specializer, and the second is the argument
                               (let ((specializer (car tuple))
                                     (argument (cadr tuple)))
                                 ;; If the specializer is a list, then it means it's a EQL specializer
                                 (if (listp specializer)
                                     (cond
                                      ;; Verify it is a EQL specializer and verify the object is the same
                                      ((eql (car specializer) 'eql) (eql (symbol-value (cadr specializer)) argument))
                                      ;; Signale error in case any it's not an EQL specializer
                                      ;; More special specializers could be added here
                                      (t (error "Invalid specializer")))
                                     ;; In case the specializer is not a list, then it is a class
                                     ;; The argument is used to verify that it is a instance of the specializer class
                                     (instancep argument (symbol-value specializer)))))
                          ;; The specializers and the arguments are zipped to create the corresponding pairs
                        (zip specializers arguments))))
          ;; Collect the method in case previous condition is true
        collect method))

;; Function to check if a specializer is more specific than another, based on a main class (the class of the argument)
(defun is-more-specific? (main-class specializer1 specializer2)
  (cond
   ;; If the first specializer is a list, it means that it is a EQL specializer
   ;; Therefore, it returns always 't, because this specializer is the most specific option, no matter what the other specializer is
   ((listp specializer1) t)
   ;; If the second specializer is a list, it means that it is a EQL specializer
   ;; Therefore, it returns always nil, because this would be more specific than any specializer
   ((listp specializer2) nil)
   ;; In case both specializers are classes, the precedence list of the main class is calculated
   (t (let ((precedence-list (class-precedence-list main-class)))
        ;; The first specializer is considered more specific than the second one if it appears first on the precedence list
        (< (position specializer1 precedence-list) (position specializer2 precedence-list))))))

;; Function to check if a list of specializers is more specific than another list, based on the arguments and the precedence order
(defun is-more-specific-list? (precedence-order arguments specializers1 specializers2)
  (if precedence-order
      ;; First, precedence-order list is used, which is a list of the indexes representing the order in which the specializers should be verified
      ;; Based on this, the class of the object passed as argument on that position is obtained
      ;; The specializers in those positions are also obtained
      (let ((main-class (object-class (nth (car precedence-order) arguments)))
            (specializer1 (nth (car precedence-order) specializers1))
            (specializer2 (nth (car precedence-order) specializers2)))
        (cond
         ;; If both specializers are the same, they are ignored and the verification continues with the next pair of specializers
         ((eql specializer1 specializer2) (is-more-specific-list? (cdr precedence-order) arguments specializers1 specializers2))
         ;; If the specializers are different, then it returns 't if the first specializer is more specific than the second one
         ;; Considering that they are different, if the first specializer is not more specific than the second one, then the second one is more specific than the first one
         (t (is-more-specific? main-class specializer1 specializer2))))
      ;; If the precedence-order list is empty, then there are no more specializers to verify and the recursion stops
      nil))

;; Function to get the most specific method from a list, based on the arguments and the precedence-order list
(defun select-most-specific-method (precedence-order arguments methods)
  ;; Iterate over all the methods and keep the most specific
  (loop with candidate = (first methods)
        for method in (rest methods)
          ;; Check if the method is more specific than the current partial most specific
          when (is-more-specific-list?
                precedence-order
                arguments
                (method-specializers method)
                (method-specializers candidate))
          ;; If this is the case, the partial most specific method is replaced
        do (setq candidate method)
        finally (return candidate)))

;; Function to call a generic function based on a list of methods and a precedence-order list
(defun call-generic-function-helper (precedence-order methods arguments)
  ;; Get the applicable methods and the obtain the most specific one
  (let* ((applicable-methods (compute-applicable-methods methods arguments))
         (most-specific-method (select-most-specific-method precedence-order arguments applicable-methods)))
    ;; Call the lambda function with the arguments
    (funcall (method-function most-specific-method)
      arguments
      (remove most-specific-method applicable-methods))))

;; Function to call the generic function including the "before" and "after" auxiliary methods
(defun call-main-generic-function (gf arguments)
  ;; If there are "before" auxiliary methods, execute them first
  (when (generic-function-before-methods gf)
        (call-generic-function-helper (generic-function-argument-precedence-order gf) (generic-function-before-methods gf) arguments))
  ;; Now, execute the main methods, and save the result
  (let ((result (call-generic-function-helper (generic-function-argument-precedence-order gf) (generic-function-methods gf) arguments)))
    ;; If there are "after" auxiliary methods, execute them now
    (when (generic-function-after-methods gf)
          (call-generic-function-helper (generic-function-argument-precedence-order gf) (generic-function-after-methods gf) arguments))
    ;; Return the result of the main methods
    result))

;; Function to call a generic function
(defun call-generic-function (gf &rest arguments)
  ;; If there are "around" methods, execute them instead of the primary methods
  (if (generic-function-around-methods gf)
      (call-generic-function-helper (generic-function-argument-precedence-order gf) (generic-function-around-methods gf) arguments)
      (call-main-generic-function gf arguments)))