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

(μ *O2*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Молекулярная масса топливного газа, кг/моль
