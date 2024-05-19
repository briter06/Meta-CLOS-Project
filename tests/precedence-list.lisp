(in-package :closless)
(load "src/utils/tests.lisp")
(load "src/miniclos/loader.lisp")


(assert-equals (class-precedence-list (find-class '*object*)) '(*object* t))

; Scenario 1

(defclass food () ())
(defclass spice (food) ())
(defclass fruit (food) ())
(defclass cinnamon (spice) ())
(defclass apple (fruit) ())
(defclass pie (apple cinnamon) ())

(assert-equals (class-precedence-list (find-class 'pie)) '(pie apple fruit cinnamon spice food *object* t))
(assert-equals (class-precedence-list (find-class 'apple)) '(apple fruit food *object* t))
(assert-equals (class-precedence-list (find-class 'cinnamon)) '(cinnamon spice food *object* t))
(assert-equals (class-precedence-list (find-class 'food)) '(food *object* t))

(unbound-classes (pie apple cinnamon fruit spice food))

; Scenario 2

(defclass cinnamon () ())
(defclass apple () ())
(defclass pastry (cinnamon apple) ())
(defclass pie (apple cinnamon) ())

(assert-equals (class-precedence-list (find-class 'pie)) '(pie apple cinnamon *object* t))
(assert-equals (class-precedence-list (find-class 'pastry)) '(pastry cinnamon apple *object* t))

(unbound-classes (pie pastry apple cinnamon))

; Scenario 3

(defclass food () ())
(defclass fruit (food) ())
(defclass apple (fruit) ())
(defclass new-class (fruit apple) ())

(assert-should-raise (class-precedence-list (find-class 'new-class)) predecence-list-error "There is a cycle in the precedence list")

(unbound-classes (new-class apple fruit food))

; Scenario 4

(defclass J () ())
(defclass L2 (J) ())
(defclass L1 (L2) ())
(defclass M1 (J) ())
(defclass R3 (J) ())
(defclass R2 (R3) ())
(defclass R1 (R2) ())
(defclass S2 (L1 M1 R1) ())
(defclass S1 (S2) ())

(assert-equals (class-precedence-list (find-class 'S1)) '(S1 S2 L1 L2 M1 R1 R2 R3 J *object* t))

(unbound-classes (S1 S2 L1 L2 M1 R1 R2 R3 J))

; Scenario 5
(defclass A () ())
(defclass B (A) ())
(defclass C (A) ())
(defclass D (A) ())
(defclass E (B) ())
(defclass F (C) ())
(defclass G (E F) ())
(defclass H (F D) ())
(defclass I (G) ())
(defclass J (H) ())
(defclass K (I J) ())

; For CLOS
; (closer-mop:finalize-inheritance (find-class 'K))
; (closer-mop:class-finalized-p (find-class 'K))
; (closer-mop:class-precedence-list (find-class 'K))

(assert-equals (class-precedence-list (find-class 'K)) '(K I G E B J H F C D A *object* t))

(unbound-classes (K I G E B J H F C D A))

(print "All the tests passed")