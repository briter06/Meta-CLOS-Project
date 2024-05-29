;; Macro to create unique symbols to use inside a macro and avoid variable name conflicts
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))