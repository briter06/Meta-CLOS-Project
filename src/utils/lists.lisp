;; Zip two lists together
(defun zip (l1 l2)
  (loop for x in l1 for y in l2 collect `(,x ,y)))

;; Get the positions of the elements in l2 based on the elements of l1
(defun get-positions (l1 l2)
  (mapcar (lambda (x) (position x l1)) l2))