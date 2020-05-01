(cl:in-package #:sicl-boot-phase-6)

;;; SUB-SPECIALIZER-P calls CLASS-PRECEDENCE-LIST to obtain the class
;;; precedence list of an argument passed to a generic function.  Then
;;; it calls POSITION to determine which of two classes comes first in
;;; that precedence list.
;;;
;;; SUB-SPECIALIZER-P is called by COMPUTE-APPLICABLE-METHODS
;;; (indirectly) to determine which is two methods is more specific.
(defun define-sub-specializer-p (environment)
  (load-fasl "CLOS/sub-specializer-p.fasl" environment))

;;; COMPUTE-APPLICABLE-METHODS calls MAPCAR (indirectly) in order to
;;; get the class of each of the arguments passed to a generic
;;; function.  It calls SORT to sort the applicable methods in order
;;; from most specific to least specific.  EQL is called to compare
;;; the object of an EQL specializer to an argument passed to a
;;; generic function.
(defun define-compute-applicable-methods (e6)
  (load-fasl "CLOS/compute-applicable-methods-support.fasl" e6)
  (load-fasl "CLOS/compute-applicable-methods-defgenerics.fasl" e6)
  (load-fasl "CLOS/compute-applicable-methods-defmethods.fasl" e6))
    
(defun define-compute-effective-method (e6)
  (load-fasl "CLOS/compute-effective-method-defgenerics.fasl" e6)
  (load-fasl "CLOS/compute-effective-method-support.fasl" e6)
  (load-fasl "CLOS/compute-effective-method-defmethods.fasl" e6))

(defun define-compute-discriminating-function (e6)
  (load-fasl "CLOS/compute-discriminating-function-defgenerics.fasl" e6)
  (define-stamp e6)
  (load-fasl "Cons/accessor-defuns.fasl" e6)
  (load-fasl "CLOS/compute-discriminating-function-support.fasl" e6)
  (load-fasl "CLOS/discriminating-tagbody.fasl" e6)
  (load-fasl "CLOS/compute-discriminating-function-support-c.fasl" e6)
  (load-fasl "CLOS/compute-discriminating-function-defmethods.fasl" e6))

(defun define-general-instance-access (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (e6 sicl-boot:e6))
      boot
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-p e6)
          (sicl-genv:fdefinition 'sicl-clos::general-instance-p e5))))

(defun define-compile (e5 e6)
  (setf (sicl-genv:fdefinition 'compile e6)
        (lambda (name &optional definition)
          (assert (null name))
          (assert (not (null definition)))
          (let* ((cst (cst:cst-from-expression definition))
                 (client (make-instance 'sicl-boot:client))
                 (ast (let ((cleavir-cst-to-ast::*origin* nil))
                        (cleavir-cst-to-ast:cst-to-ast client cst e6)))
                 (code-object (sicl-compiler:compile-ast client ast)))
            (sicl-boot:tie-code-object client code-object e6 e5)))))

(defun define-no-applicable-method (e6)
  (load-fasl "CLOS/no-applicable-method-defgenerics.fasl" e6)
  (load-fasl "CLOS/no-applicable-method.fasl" e6))

(defun define-find-accessor-method-class (e5 e6)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-accessor-method-class e6)
        (lambda (class-name &optional error-p)
          (declare (ignore error-p))
          (assert (member class-name
                          '(sicl-clos:standard-reader-method
                            sicl-clos:standard-writer-method)))
          (sicl-genv:find-class class-name e5))))

(defun define-classp (e6)
  (load-fasl "CLOS/classp-defgeneric.fasl" e6)
  (load-fasl "CLOS/classp-defmethods.fasl" e6))

(defun define-set-funcallable-instance-function (e6)
  (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function e6)
        (lambda (funcallable-instance function)
          (closer-mop:set-funcallable-instance-function
           funcallable-instance
           function))))

(defun enable-generic-function-invocation (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (e6 sicl-boot:e6))
      boot
    (define-classp e6)
    (define-sub-specializer-p e6)
    (define-compute-applicable-methods e6)
    (define-compute-effective-method e6)
    (define-no-applicable-method e6)
    (define-general-instance-access boot)
    (define-set-funcallable-instance-function e6)
    (do-symbols (symbol (find-package '#:common-lisp))
      (when (special-operator-p symbol)
        (setf (sicl-genv:special-operator symbol e6) t)))
    (setf (sicl-genv:fdefinition 'slot-value e6)
          (lambda (&rest args)
            (declare (ignore args))
            (error "slot-value called")))
    (setf (sicl-genv:fdefinition '(setf slot-value) e6)
          (lambda (&rest args)
            (declare (ignore args))
            (error "(setf slot-value) called")))
    (define-compile e5 e6)
    (load-fasl "Evaluation-and-compilation/lambda.fasl" e6)
    (load-fasl "Data-and-control-flow/setf-defmacro.fasl" e6)
    (define-find-accessor-method-class e5 e6)
    (define-compute-discriminating-function e6)))
