(defun zip (l1 l2)
  (loop for x in l1 for y in l2 collect `(,x ,y)))

(defun zip3 (l1 l2 l3)
  (loop for x in l1 for y in l2 for z in l3 collect `(,x ,y ,z)))