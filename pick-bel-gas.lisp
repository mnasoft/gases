;;;; pick-bel-gas.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

(defparameter *pick-bel-gas*
  (list
   (list *C1*	          0.9684)
   (list *C2*	          0.0179)
   (list *C3*	          0.00365)
   (list *iC4*	          0.00063)
   (list *nC4*	          0.00054)
   (list *nC5*	          0.000019)  ;; неопентан
   (list *iC5*	          0.000094)
   (list *nC5*	          0.000069)
   (list *nC6*	          0.0000119)
   (list *CO2*	          0.000128)
   (list *N2*	          0.0073)
   (list *O2*	          0.000083)
  
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(mapcar
 #'(lambda (el)
     (format t "<tr><td>~A<td><td>~A<td><td>~A<td></tr>~%"
	     (molecule-name-ru (first el))
	     (molecule-name-en-short (first el))
	     (second el)))
 *running-gas*)
