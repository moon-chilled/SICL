(cl:in-package #:common-lisp-user)

(defun load-target-macros ()
  (setf (macro-function 'target-common-lisp:defmacro)
        (macro-function 'defmacro))
  (do-external-symbols (symbol (find-package '#:common-lisp))
    (when (and (fboundp symbol)
               (null (macro-function symbol))
               (not (special-operator-p symbol)))
      (setf (fdefinition (find-symbol (symbol-name symbol) '#:target-common-lisp))
            (fdefinition symbol))))
  (load "defmacro-defmacro.lisp")
  (load "lambda-defmacro.lisp")
  (load "return-defmacro.lisp")
  (load "when-defmacro.lisp")

        
