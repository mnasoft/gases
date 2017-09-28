;;;; test.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

(μ-mixture *running-gas*)

(μ-mixture *stopping-gas*)

(k-mixture *running-gas* 353)

(k-mixture *stopping-gas* (+ 273 120))

(Cv-mixture *running-gas* 373)

(k-mixture *stopping-gas* 373)

(Cp-mixture *stopping-gas* 373)

(μCp *N2* 373)

(k *C1* 293)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Молекулярная масса топливного газа, кг/моль



(areas:circle-diameter-by-area (/ 8.61198 24.0 0.9))
