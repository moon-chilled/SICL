(in-package #:sb-vm)

(defknown buoy::single-float-unsigned-bits (single-float) (unsigned-byte 32)
  (movable foldable flushable))

#+x86-64
(define-vop (buoy::single-float-unsigned-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (unsigned-reg)))
  (:arg-types single-float)
  (:result-types unsigned-num)
  (:translate buoy::single-float-unsigned-bits)
  (:policy :fast-safe)
  (:generator 0
     (sc-case float
       (single-reg
        (inst movd bits float))
       (single-stack ; c.f. ea-for-sf-stack
        (inst mov ;'(:dword :qword)
              bits (ea (frame-byte-offset (tn-offset float)) rbp-tn)))
       (descriptor-reg
        (move bits float)
        (inst shr bits 32)))))
#+arm64
(define-vop (buoy::single-float-unsigned-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (unsigned-reg)
                  :load-if (or (sc-is float descriptor-reg single-stack)
                               (not (sc-is bits unsigned-stack)))))
  (:arg-types single-float)
  (:result-types unsigned-num)
  (:translate buoy::single-float-unsigned-bits)
  (:policy :fast-safe)         
  (:vop-var vop)
  (:generator 4
    (sc-case bits
      (unsigned-reg
       (sc-case float
         (single-reg
          (inst fmov bits float))
         (single-stack
          (inst ldr (32-bit-reg bits)
                (@ (current-nfp-tn vop)
                   (load-store-offset (ash (tn-offset float) 3)))))
         (descriptor-reg
          (inst lsr bits float 32))))
      (unsigned-stack
       (sc-case float
         (single-reg
          (storew (32-bit-reg float) (current-nfp-tn vop) (tn-offset bits)))
         ((single-stack descriptor-reg)
          ;; Fun and games: This also affects PPC, silently.
          ;; Hopefully it's a non-issue, but I'd rather have the
          ;; explicit error than a silent miscompilation. 
          (bug "Unable to extract single-float bits from ~S to ~S" float bits)))))))


#|
(defknown buoy::cvttsd2si (double-float) (signed-byte 64)
  (movable foldable flushable))

(define-vop (buoy::cvttsd2si)
  (:args (float :scs (double-reg descriptor-reg)
                ;:load-if (not (sc-is float double-stack))
                ))
  (:results (res :scs (signed-reg)))
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate buoy::cvttsd2si)
  (:policy :fast-safe)
  (:generator 0
              (inst cvttsd2si res float)))
|#

(declaim (inline buoy::ftrunc))
(declaim (ftype (function (double-float) double-float) ftrunc))

#+x86-64
(progn
  (defknown buoy::round-double (double-float (member :round :floor :ceiling :truncate))
      double-float
      (foldable flushable movable always-translatable))

  (define-vop ()
    (:translate buoy::round-double)
    (:policy :fast-safe)
    (:args (x :scs (double-reg) :target r))
    (:arg-types double-float (:constant symbol))
    (:info mode)
    (:results (r :scs (double-reg)))
    (:result-types double-float)
    (:generator 2
                (unless (location= r x) 
                  (inst xorpd r r))
                (inst roundsd r x
                      (logior #b1000
                              (ecase mode
                                (:round 0)
                                (:floor 1)
                                (:ceiling 2)
                                (:truncate 3))))))
  (defun buoy::ftrunc (y) (buoy::round-double y)))
#-x86-64
(defun buoy::ftrunc (y) (ftruncate y))

#+arm64
(progn
  (in-package #:sb-vm)
  (defknown buoy::fma-double (double-float double-float double-float) double-float
      (movable foldable flushable))

  (define-vop (buoy::fma-double)
    (:translate buoy::fma-double)
    (:policy :fast-safe)
    (:args (accumulator :scs (double-reg))
           (multiplier :scs (double-reg))
           (multiplicand :scs (double-reg)))
    (:arg-types double-float double-float double-float)
    (:results (res :scs (double-reg)))
    (:result-types double-float)
    (:generator 2
                (inst fmadd res multiplier multiplicand accumulator)))

  (defun buoy::fma-double (x y z) (buoy::fma-double x y z)))
