(cl:in-package #:sicl-boot-phase-3)

(defun define-stamp (e3)
  (flet ((unique-number (class-name)
           (funcall (sicl-genv:fdefinition 'sicl-clos::unique-number e3)
                    (sicl-genv:find-class class-name e3))))
    (setf (sicl-genv:fdefinition 'sicl-clos::stamp e3)
          (lambda (object)
            (cond ((integerp object) (unique-number 'fixnum))
                  ((consp object) (unique-number 'cons))
                  ((null object) (unique-number 'null))
                  ((symbolp object) (unique-number 'symbol))
                  ((typep object 'header) (aref (slot-value object '%rack) 0))
                  (t (error "Can't deal with object ~s" object)))))))
