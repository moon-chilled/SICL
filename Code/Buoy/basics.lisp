(cl:in-package #:buoy)

;; basic floating-point functionality

;; convenient abbreviations, since we use these a lot
(declaim (inline b2s b2d s2b d2b))
(defun b2s (x) (float-features:bits-single-float x))
(defun b2d (x) (float-features:bits-double-float x))
(defun s2b (x) (float-features:single-float-bits x))
(defun d2b (x) (float-features:double-float-bits x))

;; copysign: return a number with the same magnitude as x, but the sign of y
;; like a commuted cl float-sign
(declaim (ftype (function (single-float single-float) single-float) single-copysign))
(declaim (ftype (function (double-float double-float) double-float) double-copysign))
(defun single-copysign (x y)
  (let ((xb (s2b x))
        (yb (s2b y)))
    (b2s (dpb xb (byte 31 0) yb))))
(defun double-copysign (x y)
  (let ((xb (d2b x))
        (yb (d2b y)))
    (b2d (dpb xb (byte 63 0) yb))))

;; next-before: return the greatest floating-point less than float, with the same representation
;; next-after: return the least floating-point greater than float, with the same representation
(declaim (ftype (function ((and single-float (not (eql #.most-negative-single-float))))
                          (and single-float (not (eql #.most-positive-single-float))))
                single-next-before)
         (ftype (function ((and single-float (not (eql #.most-positive-single-float))))
                          (and single-float (not (eql #.most-negative-single-float))))
                single-next-after)
         (ftype (function ((and double-float (not (eql #.most-negative-double-float))))
                          (and double-float (not (eql #.most-positive-double-float))))
                double-next-before)
         (ftype (function ((and double-float (not (eql #.most-positive-double-float))))
                          (and double-float (not (eql #.most-negative-double-float))))
                double-next-after))
;; TODO: if there were an implicitly sign-extended version of float-bits, this would be nicer
;; (logior 1 (ash bits -31)) or so
(defun single-next-before (float)
  (if (zerop float)
      least-negative-single-float
      (let ((bits (s2b float)))
        (b2s (+ bits (if (plusp float) -1 1))))))
(defun single-next-after (float)
  (let ((bits (s2b (+ 0s0 float)))) ; (eql 0s0 (+ 0s0 -0s0))
    (b2s (+ bits (if (not (minusp float)) 1 -1)))))
(defun double-next-before (float)
  (if (zerop float)
      least-negative-double-float
      (let ((bits (d2b float)))
        (b2d (+ bits (if (plusp float) -1 1))))))
(defun double-next-after (float)
  (let ((bits (d2b (+ 0s0 float))))
    (b2d (+ bits (if (not (minusp float)) 1 -1)))))
    
;; decode-float: return (values significand exponent sign)
(declaim (ftype (function (single-float) (values
                                          (or (eql 0s0)
                                              (single-float 0.5s0 (1s0)))
                                          (integer -148 128)
                                          (member -1s0 1s0)))
                decode-single-float)
         (ftype (function (double-float) (values
                                          (or (eql 0d0)
                                              (double-float 0.5d0 (1d0)))
                                          (integer -1073 1024)
                                          (member -1d0 1d0)))
                decode-double-float))

(defun decode-single-float (float)
  (let ((bits (s2b float)))
    ;; check not denormal (all-zero exponent)
    (if (ldb-test (byte 8 23) bits)
        ;; Replace the exponent with 126 (that is a biased -1).  Since a normalised ieee
        ;; fraction has range [1 2), this brings it into the desired [0.5 1) range.
        ;; While we're at it, clear out the sign bit, that is the most significant bit.
        (let ((significand (b2s
                            (dpb 126 (byte 9 23) bits)))
              (exponent (+ -126 (ldb (byte 8 23) bits)))
              (sign (if (logbitp 31 bits) -1s0 1s0)))
          (values significand exponent sign))
        (if (zerop float)
            ;; zero; must be handled specially.  Logior is cheaper than copysign here, since we
            ;; know all the requisite bits are zero anyway; the only significant bit is the sign bit
            (values 0s0 0 (b2s (logior bits (s2b 1s0))))
            ;; denormal
            (let* ((significand-bits (ldb (byte 23 0) bits))
                   (significand-length (integer-length significand-bits))
                   (significand-leading-zeroes (- 23 significand-length))
                   (exponent (- (ldb (byte 8 23) bits) 126 significand-leading-zeroes))
                   (sign (if (logbitp 31 bits) -1s0 1s0)))
              (values
               (b2s
                (dpb 126 (byte 9 23)
                     (ash significand-bits (1+ significand-leading-zeroes))))
               exponent sign))))))

(defun decode-double-float (float)
  (let ((bits (d2b float)))
    ;; check for all-zero exponent (denormal)
    (if (ldb-test (byte 11 52) bits)
        ;; replace the exponent (8 bits starting at bit 23, past the mantissa)
        ;; with 126 (that is a biased -1).  Since an ieee fraction has range [1 2), this
        ;; brings it into the desired [0 1) range.
        ;; At the same time, clear out the sign bit, that is the most significant bit.
        (let ((significand (b2d
                            (dpb 1022 (byte 12 52) bits)))
              (exponent (+ -1022 (ldb (byte 11 52) bits)))
              (sign (if (logbitp 63 bits) -1d0 1d0)))
          (values significand exponent sign))
        (if (zerop float)
            ;; zero; must be handled specially.  Logior is cheaper than copysign here, since we
            ;; know all the requisite bits are zero anyway; the only significant bit is the sign bit
            (values 0d0 0 (b2d (logior bits (d2b 1d0))))
            ;; denormal
            (let* ((significand-bits (ldb (byte 52 0) bits))
                   (significand-length (integer-length significand-bits))
                   (significand-leading-zeroes (- 52 significand-length))
                   (exponent (- (ldb (byte 11 52) bits) 1022 significand-leading-zeroes))
                   (sign (if (logbitp 63 bits) -1d0 1d0)))
              (values
               (b2d
                (dpb 1022 (byte 12 52)
                     (ash significand-bits (1+ significand-leading-zeroes))))
               exponent sign))))))

(declaim (ftype (function (single-float) (values
                                          (integer 0 #.(1- (expt 2 24)))
                                          (integer * *)
                                          (member -1 1)))
                integer-decode-single-float)
         (ftype (function (double-float) (values (integer 0 #.(1- (expt 2 53)))
                                                 (integer * *)
                                                 (member -1 1)))
                integer-decode-double-float))

;; TODO: a sign-extended float-bits would be nice here too
(defun integer-decode-single-float (float)
  (let* ((bits (s2b float))
         (significand (ldb (byte 23 0) bits))
         (biased-exponent (ldb (byte 8 23) bits))
         (sign (if (logbitp 31 bits) -1 1))
         (normal-bit (if (zerop biased-exponent) 0 1))
         ;; realise virtual leading bit if necessary
         (significand (logior significand (ash normal-bit 23))))
    ;; subtract 22 instead of 23, but then additionally subtract 1 in the normal case
    (values significand (- biased-exponent 127 22 normal-bit) sign)))
(defun integer-decode-double-float (float)
  (let* ((bits (d2b float))
         (significand (ldb (byte 52 0) bits))
         (biased-exponent (ldb (byte 11 52) bits))
         (sign (if (logbitp 63 bits) -1 1))
         (normal-bit (if (zerop biased-exponent) 0 1))
         ;; realise virtual leading bit if necessary
         (significand (logior significand (ash normal-bit 52))))
    ;; subtract 51 instead of 52, but then additionally subtract 1 in the normal case
    (values significand (- biased-exponent 1023 51 normal-bit) sign)))

;; float-precision: return the number of significant bits (including virtual)
(declaim (ftype (function (single-float) (integer 0 24)) single-float-precision)
         (ftype (function (double-float) (integer 0 53)) double-float-precision))
(defun single-float-precision (float)
  (let* ((bits (s2b float))
         (significand-length (integer-length (ldb (byte 23 0) bits))))
    (if (ldb-test (byte 8 23) bits) 24 significand-length)))
(defun double-float-precision (float)
  (let* ((bits (d2b float))
         (significand-length (integer-length (ldb (byte 52 0) bits))))
    (if (ldb-test (byte 11 52) bits) 53 significand-length)))

