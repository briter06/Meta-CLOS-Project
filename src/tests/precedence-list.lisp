(defclass food () ())
(defclass spice (food) ()) 
(defclass fruit (food) ()) 
(defclass cinnamon (spice) ())
(defclass apple (fruit) ()) 
(defclass pie (apple cinnamon) ()) 