(cl:in-package #:sicl-new-boot-phase-2)

(defun create-mop-classes (boot)
  (with-accessors ((e2 sicl-new-boot:e2))
      boot
    (load-file "CLOS/t-defclass.lisp" e2)
    (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e2 t)
          (sicl-genv:find-class 't e2))
    (load-file "CLOS/function-defclass.lisp" e2)
    (load-file "CLOS/standard-object-defclass.lisp" e2)
    (load-file "CLOS/metaobject-defclass.lisp" e2)
    (load-file "CLOS/method-defclass.lisp" e2)
    (load-file "CLOS/standard-method-defclass.lisp" e2)
    (load-file "CLOS/standard-accessor-method-defclass.lisp" e2)
    (load-file "CLOS/standard-reader-method-defclass.lisp" e2)
    (load-file "CLOS/standard-writer-method-defclass.lisp" e2)
    (load-file "CLOS/slot-definition-defclass.lisp" e2)
    (load-file "CLOS/standard-slot-definition-defclass.lisp" e2)
    (load-file "CLOS/direct-slot-definition-defclass.lisp" e2)
    (load-file "CLOS/effective-slot-definition-defclass.lisp" e2)
    (load-file "CLOS/standard-direct-slot-definition-defclass.lisp" e2)
    (load-file "CLOS/standard-effective-slot-definition-defclass.lisp" e2)
    (load-file "CLOS/method-combination-defclass.lisp" e2)
    (load-file "CLOS/specializer-defclass.lisp" e2)
    (load-file "CLOS/eql-specializer-defclass.lisp" e2)
    (load-file "CLOS/class-unique-number-defparameter.lisp" e2)
    (load-file "CLOS/class-defclass.lisp" e2)
    (load-file "CLOS/forward-referenced-class-defclass.lisp" e2)
    (load-file "CLOS/real-class-defclass.lisp" e2)
    (load-file "CLOS/regular-class-defclass.lisp" e2)
    (load-file "CLOS/standard-class-defclass.lisp" e2)
    (load-file "CLOS/funcallable-standard-class-defclass.lisp" e2)
    (load-file "CLOS/built-in-class-defclass.lisp" e2)
    (load-file "CLOS/funcallable-standard-object-defclass.lisp" e2)
    (load-file "CLOS/generic-function-defclass.lisp" e2)
    (load-file "CLOS/standard-generic-function-defclass.lisp" e2)
    (load-file "Cons/cons-defclass.lisp" e2)
    (load-file "Sequences/sequence-defclass.lisp" e2)
    (load-file "Cons/list-defclass.lisp" e2)
    (load-file "Package-and-symbol/symbol-defclass.lisp" e2)
    (load-file "Arithmetic/number-defclass.lisp" e2)
    (load-file "Arithmetic/real-defclass.lisp" e2)
    (load-file "Arithmetic/rational-defclass.lisp" e2)
    (load-file "Arithmetic/integer-defclass.lisp" e2)
    (load-file "Arithmetic/fixnum-defclass.lisp" e2)))
