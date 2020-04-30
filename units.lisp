(defpackage :lass-units
  (:use #:cl
        #:lass
        #:parse-float
        #:physical-quantities)
  (:export #:resolve-css-arg
           #:with-css-units))

(in-package :lass-units)

;;; Using physical-quantities to define units of measure relevant to CSS
(defmacro with-css-units (&body body)
  `(with-local-units
     (define-unit-prefix |yocto| -24 :abbrev |y|)
     (define-unit-prefix |zepto| -21 :abbrev |z|)
     (define-unit-prefix |atto|  -18 :abbrev |a|)
     (define-unit-prefix |femto| -15 :abbrev |f|)
     (define-unit-prefix |pico| -12 :abbrev |p|)
     (define-unit-prefix |nano| -9 :abbrev |n|)
     (define-unit-prefix |micro| -6 :abbrev |u|)
     (define-unit-prefix |milli| -3 :abbrev |m|)
     (define-unit-prefix |centi| -2 :abbrev |c|)
     (define-unit-prefix |deci| -1 :abbrev |d|)
     (define-unit-prefix |deca| 1 :abbrev |da|)
     (define-unit-prefix |hecto| 2 :abbrev |h|)
     (define-unit-prefix |kilo| 3 :abbrev |k|)
     (define-unit-prefix |mega| 6 :abbrev |M|)
     (define-unit-prefix |giga| 9 :abbrev |G|)
     (define-unit-prefix |tera| 12 :abbrev |T|)
     (define-unit-prefix |peta| 15 :abbrev |P|)
     (define-unit-prefix |exa| 18 :abbrev |E|)
     (define-unit-prefix |zetta| 21 :abbrev |Z|)
     (define-unit-prefix |yotta| 24 :abbrev |Y|)
     (define-unit-prefix |kibi| 1 :abbrev |Ki| :base 1024)
     (define-unit-prefix |mebi| 2 :abbrev |Mi| :base 1024)
     (define-unit-prefix |gibi| 3 :abbrev |Gi| :base 1024)
     (define-unit-prefix |tebi| 4 :abbrev |Ti| :base 1024)
     (define-unit-prefix |pebi| 5 :abbrev |Pi| :base 1024)
     (define-unit-prefix |exbi| 6 :abbrev |Ei| :base 1024)
     (define-unit-prefix |zebi| 7 :abbrev |Zi| :base 1024)
     (define-unit-prefix |yobi| 8 :abbrev |Yi| :base 1024)

     ;; Most units are named according to their name in CSS
     (define-unit |m| :alias (|meter| |metres| |meters|) :prefix-test (pq::prefix-range 10 nil 3))
     (define-unit |s| :alias (|second| |seconds|) :prefix-test (pq::prefix-range 10 nil -3))
     (define-unit |rad| :def (1) :alias (|radian| |radians|) :prefix-test (pq::prefix-range 10 nil -3))
     (define-unit |grad| :def (400/360 |rad|) :alias (|gradian| |gradians|) :prefix-test (pq::prefix-range 10 nil -3))
     (define-unit |turn| :def ((* 2 pi) |rad|) :alias |turns| :prefix-test (constantly nil))
     (define-unit |steradian| :def (1) :abbrev |sr| :prefix-test (pq::prefix-range 10 nil -3))
     (define-unit |Hz| :def (1 / |second|) :alias |hertz|  :prefix-test (pq::prefix-base 10 3))
     (define-unit |byte| :def (1) :alias |bytes| :abbrev |b| :prefix-test (pq::prefix-or (pq::prefix-base 1024) (pq::prefix-range 10 3 nil)))
     (define-unit |minute| :def (60 |s|) :alias |minutes| :abbrev |min| :prefix-test (constantly nil))
     (define-unit |hour| :def (60 |minute|) :alias |hours| :abbrev |h| :prefix-test (constantly nil))
     (define-unit |day| :def (24 |hour|) :alias |days| :abbrev |d| :prefix-test (constantly nil))
     (define-unit |deg| :def ((/ pi 180) |rad|) :alias (|degrees| |degree|) :prefix-test (pq::prefix-range 10 nil -3))
     (define-unit |%| :def (0.01) :alias (|percent| |percents|) :prefix-test (constantly nil))
     (define-unit |in| :def (0.0254 |m|) :alias (|inch| |inches|) :prefix-test (constantly nil))
     (define-unit |pt| :def (1/72 |inch|) :alias (|point| |points|) :prefix-test (constantly nil))
     (define-unit |pc| :def (1/6 |inch|) :alias (|pica| |picas|) :prefix-test (constantly nil))
     (define-unit |px| :def (1/96 |inch|) :alias (|pixel| |pixels|) :prefix-test (constantly nil))
     ;; assuming the most common font size, used only in mixed calculations
     (define-unit |em| :def (16 |px|) :prefix-test (constantly nil))

     ,@body))

(defun parse-css-number (x &key unit)
  "Parses CSS number wrapping it in pq:quantity if unit of measure is present"
  (cond ((stringp x)
         (multiple-value-bind (val idx) (parse-float x :junk-allowed t)
           (let ((q-unit (cond (unit unit)
                               ((>= idx (length x)) nil)
                               ((char-equal #\e (char x (1- idx))) (subseq x (1- idx)))
                               (t (subseq x idx)))))
             (if q-unit
                 (make-quantity :value val :unit (make-unit (list q-unit 1)))
                 val))))
        ((symbolp x)
         (parse-css-number (string x) :unit unit))
        (unit
         (make-quantity :value x :unit (make-unit (list unit 1))))
        (t
         x)))

(defun reduce-percents (q)
  "Collapses percents in quantity so that 100% * 100% = 100% and not 10000%^2"
  (if (quantityp q)
      (let ((units (unit q))
            (value (value q)))
        (multiple-value-bind (percents others)
            (loop for u in units
                  if (string= "%" (uf-unit u))
                    collect u into percents
                  else collect u into others
                  finally (return (values percents others)))
          (cond ((not percents) q)
                ((not others)
                 (let ((power (- (uf-power (car percents)) 1)))
                   (make-quantity :value (/ value (expt 100.0 power))
                                  :unit (make-unit (list "%" (max 1 power))))))
                (t (make-quantity :value (/ value (expt 100.0 (uf-power (car percents))))
                                  :unit others)))))
      q))

(defun resolve-css-number (x)
  "Returns CSS number with unit, only power 1 units are properly supported."
  (if (quantityp x)
      (let* ((q (reduce-percents x))
             (value (value q))
             (units (unit q))
             (unit (when units (uf-unit (car units)))))
        (values (if (string= "%" unit) (/ value 100.0) value) unit value))
      (values x nil x)))

(defun css-number-string-format (value unit raw)
  "Render a quantity with unit in CSS format"
  (cond ((string= "%" unit)
         (format nil "~$%" raw))
        (unit
         (format nil "~$~A" value unit))
        ((integerp value)
         (format nil "~A" value))
        (t
         (format nil "~$" value))))

(defun css-number-string (q)
  (multiple-value-call 'css-number-string-format (resolve-css-number q)))

(defun wrap-funcall (fn q)
  (with-css-units
    (multiple-value-bind (value unit raw) (resolve-css-number q)
      (declare (ignore raw))
      (let ((result (funcall fn value)))
        (values result unit (if (string= "%" unit) (* 100.0 result) result))))))

(defun wrap-funcall-raw (fn q)
  (with-css-units
    (multiple-value-bind (value unit raw) (resolve-css-number q)
      (declare (ignore value))
      (let ((result (funcall fn raw)))
        (values (if (string= "%" unit) (/ result 100.0) result) unit result)))))

(defun resolve-css-arg (expr)
  "Utility function to get the numeric value of the value with unit of measure"
  (with-css-units
    (resolve-css-number
     (parse-css-number
      (resolve expr)))))

;;; Convert PQ function with 'q' prefix to a LASS propery function by reading and resolving CSS values and properly
;;; formatting the result

(defmacro define-css-op (op)
  `(define-property-function ,(string op) (&rest args)
     (with-css-units
       (css-number-string
        (apply #',(intern (string-upcase (concatenate 'string "q" (string op))))
               (mapcar (lambda (x) (parse-css-number (resolve x))) args))))))

;;; Assign unit to a number
(define-property-function unit (value &optional unit)
  (with-css-units
    (let ((numeric-value (resolve-css-number
                          (parse-css-number
                           (resolve value)))))
      (css-number-string
       (make-quantity :value (if (string= "%" unit)
                                 (* numeric-value 100)
                                 numeric-value)
                      :unit (when unit (make-unit (list unit 1))))))))

;;; Convert value to the specified unit if compatible
(define-property-function convert-unit (value unit)
  (with-css-units
    (let ((numeric-value (parse-css-number
                          (resolve value))))
      (css-number-string
       (convert-unit (if (quantityp numeric-value)
                         numeric-value
                         (make-quantity :value numeric-value))
                     (list (list unit 1)))))))

;;; Adding math functions
(define-css-op +)
(define-css-op -)
(define-css-op /)
(define-css-op *)
(define-css-op =)
(define-css-op /=)
(define-css-op >)
(define-css-op <)
(define-css-op <=)
(define-css-op >=)
(define-css-op round)
(define-css-op ln)
(define-css-op log)
(define-css-op exp)
(define-css-op expt)
(define-css-op root)
(define-css-op sqrt)
(define-css-op pow)
(define-css-op sin)
(define-css-op asin)
(define-css-op sinh)
(define-css-op asinh)
(define-css-op cos)
(define-css-op acos)
(define-css-op cosh)
(define-css-op acosh)
(define-css-op tan)
(define-css-op atan)
(define-css-op tanh)
(define-css-op atanh)
(define-css-op abs)

(defun qclamp (min-value value max-value)
  "Value clamping function"
  (cond
    ((q< value min-value)
     min-value)
    ((q> value max-value)
     max-value)
    (t value)))

(define-css-op clamp)

(defun qhypot (&rest args)
  "Function returns a sum of squares of its arguments"
  (reduce #'q+ args :key (lambda (x) (qpow x 2))))

(define-css-op hypot)

(defun qrandom (&optional (range 1.0))
  "Generates random number, if range is specified as a quantity it's value will be used as parameter to CL:RANDOM and unit would be used for resulting quantity"
  (q* (random 1.0) range))

(define-css-op random)

;;; Rounds 45.5% to 46%
(define-property-function ceil (a)
  (with-css-units
    (multiple-value-call 'css-number-string-format
      (wrap-funcall-raw #'ceiling (parse-css-number (resolve a))))))

(define-property-function floor (a)
  (with-css-units
    (multiple-value-call 'css-number-string-format
      (wrap-funcall-raw #'floor (parse-css-number (resolve a))))))


