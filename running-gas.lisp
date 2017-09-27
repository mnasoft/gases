;;;; running-gas.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

(defparameter *running-gas*
  (list
    (list *N2*	          0.0003)
    (list *CO2*	          0.0022)
    (list *C1*	          0.7374)
    (list *C2*	          0.0593)
    (list *C3*	          0.1179)
    (list *iC4*	          0.0131)
    (list *nC4*	          0.0379)
    (list *iC5*	          0.0130)
    (list *nC5*	          0.0139)
    (list *nC6*	          0.0017)
    (list *Mcyclo_C5*	  0.0004)
    (list *Cyclo_C6*	  0.0002)
    (list *nC7*	          0.0001)
    (list *Mcyclo_C6*	  0.0001)
    (list *H2O*	          0.0027)))
