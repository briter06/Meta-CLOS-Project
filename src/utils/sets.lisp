;; Concatenate two sets, where the :test function is used to verify if an element already exists on the set and skip it
(defun concat-sets (set1 set2 &key test)
  (reduce (lambda (acc x) (adjoin x acc :test (or test #'equal))) set1 :initial-value set2))

;; Remove element from a set
(defun remove-from-set (elem set)
  (remove-if (lambda (x) (eql x elem)) set))