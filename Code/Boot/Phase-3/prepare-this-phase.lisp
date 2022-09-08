(cl:in-package #:sicl-boot-phase-3)

(defun prepare-this-phase (e1 e2 e3)
  (setf (env:fdefinition (env:client e3) e3 'sicl-boot:ast-eval)
        (lambda (client ast)
          (sicl-ast-evaluator:eval-ast client e3 ast)))
  (sicl-boot:copy-macro-functions e2 e3)
  (ensure-asdf-system '#:sicl-data-and-control-flow-type-proclamations e3)
  (ensure-asdf-system '#:sicl-evaluation-and-compilation-proclamations e3)
  (ensure-asdf-system '#:sicl-cons-type-proclamations e3)
  (ensure-asdf-system '#:sicl-arithmetic-type-proclamations e3)
  (ensure-asdf-system '#:sicl-conditions-type-proclamations e3)
  (ensure-asdf-system '#:sicl-symbol-type-proclamations e3)
  (ensure-asdf-system '#:sicl-type-type-proclamations e3)
  (ensure-asdf-system '#:sicl-character-type-proclamations e3)
  (ensure-asdf-system '#:sicl-sequence-type-proclamations e3)
  (ensure-asdf-system '#:sicl-run-time-type-proclamations e3)
  (ensure-asdf-system '#:sicl-clos-type-proclamations e3)
  (ensure-asdf-system '#:sicl-string-type-proclamations e3)
  (ensure-asdf-system '#:cleavir-code-utilities-type-proclamations e3)
  (ensure-asdf-system '#:sicl-array-proclamations e3)
  (ensure-asdf-system '#:sicl-package-proclamations e3)
  (enable-typep e2)
  (enable-object-creation e1 e2)
  (enable-defgeneric e1 e2 e3)
  (enable-class-initialization e3)
  (enable-defclass e1 e2 e3)
  (enable-defmethod e1 e2 e3))
