(defpackage :closless
  (:use :common-lisp)
  (:shadow
   ADD-METHOD ALLOCATE-INSTANCE BUILT-IN-CLASS CALL-METHOD CALL-NEXT-METHOD
   CHANGE-CLASS CLASS CLASS-NAME CLASS-OF COMPUTE-APPLICABLE-METHODS
   DEFCLASS DEFGENERIC DEFINE-METHOD-COMBINATION DEFMETHOD DESCRIBE-OBJECT ENSURE-GENERIC-FUNCTION
   FIND-CLASS FIND-METHOD GENERIC-FUNCTION INITIALIZE-INSTANCE INVALID-METHOD-ERROR
   MAKE-INSTANCE MAKE-INSTANCES-OBSOLETE MAKE-LOAD-FORM-SAVING-SLOTS MAKE-METHOD METHOD
   METHOD-COMBINATION METHOD-COMBINATION-ERROR METHOD-QUALIFIERS NEXT-METHOD-P
   NO-APPLICABLE-METHOD NO-NEXT-METHOD PRINT-NOT-READABLE-OBJECT
   PRINT-OBJECT PRINT-UNREADABLE-OBJECT REINITIALIZE-INSTANCE REMOVE-METHOD
   SLOT-BOUNDP SLOT-EXISTS-P SLOT-MAKUNBOUND SLOT-MISSING SLOT-UNBOUND SLOT-VALUE
   STANDARD-CLASS STANDARD-GENERIC-FUNCTION STANDARD-METHOD STANDARD-OBJECT
   STRUCTURE-CLASS STRUCTURE-OBJECT UNBOUND-SLOT UNBOUND-SLOT-INSTANCE
   UPDATE-INSTANCE-FOR-DIFFERENT-CLASS UPDATE-INSTANCE-FOR-REDEFINED-CLASS
   WITH-ACCESSORS WITH-SLOTS))