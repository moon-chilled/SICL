(cl:in-package #:buoy)

;; The cosine and sine functions, for single floats.  Adapted from RLIBM.

;; Copyright (c) 2022 Sehyeok Park, Mridul Aanjaneya, and Santosh
;; Nagarakatte, Rutgers Architecture and Programming Languages (RAPL)
;; Group
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;; little-endian coefficients
;; for even, first coeff is constant (x^0); for odd, first coeff is for x^1
(defmacro even-polynomial (type x &rest coeffs)
  (let ((xs (gensym))
        (x2 (gensym)))
    `(let* ((,xs (the ,type ,x))
            (,x2 (* ,xs ,xs)))
       ,(loop for coeff in (mapcar (lambda (y) `(the ,type ,y)) (reverse coeffs))
              for form = coeff then `(fma-double ,coeff ,x2 ,form)
              finally (return form)))))

(defmacro odd-polynomial (type x &rest coeffs)
  (let ((xs (gensym)))
    `(let* ((,xs (the ,type ,x)))
       (* ,xs (even-polynomial ,type ,xs ,@coeffs)))))
  

(defconstant +pi/256+ (b2d #x3f8921fb54442d18))

(declaim (ftype (function (single-float) (single-float -1s0 1s0)) cos-single sin-single))

(defun cos-single (x &aux (xi (logand #x7fffffff (single-float-unsigned-bits x))))
  (declare (optimize (speed 3) (safety 0)))
  (cond
    ((< xi #x39800001) 1s0) ; that is ~2.44e-4; floats in that range round to 1
    ;((>= xi #x7f800000) (- x x)) ; infinities and nans live there.  NB. this can be handled w/o a branch using avx512 fixup.  Or we can ensure the following computations induce nan/trap for inf/nan input?
    ((< xi #x3C490FDB) ; <~0.0123; for numbers in this range, we approximate using a low-degree polynomial
     (let* ((xd (coerce x 'double-float))
            (x2 (* xd xd)))
       (coerce
        (fma-double (b2d #x3ff0000000000002)
                    x2
                    (fma-double (b2d #xbfe00000000a7c61)
                                x2
                                (b2d #x3fa5558e8686159f)))
        'single-float)))
    (t
     ;; slower path; start with range reduction
     ;; reduce x modulo 256*pi
     (let ((xd (coerce (abs x) 'double-float))) ; xi was abs'd, but x was not yet; fine for the above, since it's immediately squared, but not here
       (multiple-value-bind (q r) ; quotient and fractional remainder after division by pi
           (if (< xi #x4a000000)
               ;; <2097152; numbers in this range can be range-reduced with a lower-precision pi constant
               (let* ((q (the (double-float 1d0 1.8d8) (* xd (b2d #x40545f306c000000)))) ; first quotient approximation; multiply by ~256/pi
                      (tq (round-double q :truncate))                  ; truncated quotient
                      (iq (the (integer 0 170891318) (truncate q))) ; integer quotient
                      (r (- q tq))                        ; remainder estimate
                      (r (fma-double r xd (b2d #x3e9c9c882a53f84f)))) ; refine remainder estimate using some lower bits of 256/pi
                 (if (> r 1d0)                            ; account if we overestimated (todo should be >=?)
                     (values (1+ iq) (1- r))
                     (values iq r)))
               ;; >=2097152; numbers in this range need a higher-precision pi constant
               ;; early-out on what I assume are hard-to-round cases
               (cond ((eql xi #x5922AA80) (return-from cos-single (b2s #x3f08aebf)))
                     ((eql xi #x6A127977) (return-from cos-single (b2s #x3ed7ae35)))
                     ((eql xi #x7908CD73) (return-from cos-single (b2s #x3f798bb5)))
                     ((eql xi #x7A38AB34) (return-from cos-single (b2s #x3f7b3195)))
                     (t ;todo this branch is broken
                      (let* ((e (+ -127 (ldb (byte 7 23) xi)))
                               ;; extract mantissa.  Will always be normal, since we've already taken care of the denormal range, so this is cheaper than integer-decode-float
                               (m (logior (ash 1 23) (ldb (byte 23 0) xi)))
                               ;; 256-bit fixedpoint pi.  Might be a bit faster to do the math by hand, as the c version does, but meh
                               (qb (* m #xa2f9836e4e441529fc2757d1f534ddc0db6295993c439041fe5163abdebbc562))
                               (i (- 162 e))
                               (q (ldb (byte 64 (+ 64 i)) qb))
                               (r (ldb (byte 64 i) qb)))
                          (values q (* (coerce r 'double-float) #.(scale-float 1d0 -64)))))))  ; r 0.64 fixedpoint->fractional
;for 0.3, have 3fdc8e8b692c3d26; should be 3fdc8e8b692c3d27
         ;; defractionalise remainder
         (let ((r (* r +pi/256+))
               (q (+ 128 q))) ;why do we add 128 here?
           ;; now we have x = r+πq/256.  Compute cos(x) = cos(r+πq/256) = cos(r-(-πq/256)) = sin(r)sin(πq/256)+cos(r)cos(πq/256).  r is handled by polynomial approximation, and q by table lookup
           ;; grab low 7 bits, i.e. x divided by and mod (quantised to π/256) π/2
           (multiple-value-bind (qq qr) (floor q 128)
             ;; we want to use qr as an index into the lookup table.  If the low bit of qq is set then we are on the 'falling' portion of the cosine wave, and actually want to index with 128-qr, (and commensurately replace r with π/256-r); do this with bithacks
             (let* ((r (if (logbitp 0 qq) (- +pi/256+ r) r))
                    ;; (logand 128 q) will be 128 iff (lognot (1- (logand 1 qq))) is -1, but I don't think sbcl will figure this out
                    (qi (the (integer 0 127)
                            (+ (logand 128 q)
                               (logxor qr (lognot (1- (logand 1 qq))))))))
               ;; don't need to account for the sign of cosq/sinq; we will correct it in the result
               (let (
                     ;(sinq (aref +double-cos-*256+ qi))
                     ;(cosq (aref +double-sin-*256+ qi))
                     ;(sinq (aref +double-sin-cos-*256+ qi 1))
                     ;(cosq (aref +double-sin-cos-*256+ qi 0))
                     (sinq (aref +double-sin-cos-flat-*256+ (1+ (* 2 qi))))
                     (cosq (aref +double-sin-cos-flat-*256+ (* 2 qi)))
                     )
                 (let ((sinr (odd-polynomial double-float r #.(b2d #x3ff0000000000001) #.(b2d #xbfc55555555d3760) #.(b2d #x3f81111de524b6f0))) ; ~ x - 1/6x³ + 1/120x⁵
                       (cosr (even-polynomial double-float r #.(b2d #x3feffffffffffffc) #.(b2d #xbfdffffffff83643) #.(b2d #x3fa555488594da9d)))) ; ~ 1 - 1/2x² + 1/24x⁴
                   (* (if (logbitp 1 qq) -1s0 1s0)
                      (+ (- x x)
                      (coerce
                       (fma-double (* cosq cosr) sinq sinr)
                       'single-float)))))))))))))
             
