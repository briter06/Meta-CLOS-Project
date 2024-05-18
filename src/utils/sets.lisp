(defun concat-sets (set1 set2 &key test)
  (reduce (lambda (acc x) (adjoin x acc :test (or test #'equal))) set1 :initial-value set2))