;;;; stopping-gas.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

(defparameter *stopping-gas*
  (list (list *N2*	          0.0002)
	(list *CO2*	          0.0030)
	(list *C1*	          0.4042)
	(list *C2*	          0.0658)
	(list *C3*	          0.2469)
	(list *iC4*	          0.0506)
	(list *nC4*	          0.1621)
	(list *iC5*	          0.0359)
	(list *nC5*	          0.0277)
	(list *nC6*	          0.0009)
	(list *Mcyclo_C5*	  0.0002)
	(list *Cyclo_C6*	  0.0001)
	(list *nC7*	          0.0000)
	(list *Mcyclo_C6*	  0.0000)
	(list *H2O*	          0.0024)))
