;;;; running-gas.lisp

(in-package :gases)

;;; "gases" goes here. Hacks and glory await!

(defparameter *running-gas*
  (make-instance-composition
   '(("N2"              0.0003)
     ("CO2"             0.0022)
     ("CH4"             0.7374)
     ("C2H6"            0.0593)
     ("C3H8"            0.1179)
     ("C4H10,isobutane" 0.0131)
     ("C4H10,n-butane"  0.0379)
     ("C5H12,i-pentane" 0.0130)
     ("C5H12,n-pentane" 0.0139)
     ("C6H14,n-hexane"  0.0017)
     ("C5H10,cyclo-"    0.0004)
     ("C6H12,cyclo-"    0.0002)
     ("C7H16,n-heptane" 0.0001)
     ("C6H12,cyclo-"    0.0001)
     ("H2O"             0.0025))))

(molar-fraction-summ *running-gas*)

(defparameter *Air*
  (make-instance-composition
   `(("N2" ,(/ 78.084 100))
     ("O2" ,(/ 20.9476 100))
     ("Ar" ,(/ .9365 100))
     ("CO2",(/ .0319 100)))))

(molar-fraction-summ *Air*)

(mix-composition *Air* 1.0 *running-gas* 1.0)

(maphash #'(lambda (key value) (format t "~A~%" key)) *sp-db*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (format t "<table>~%")
  (mapcar
   #'(lambda (el)
       (format t "<tr><td>~A</td><td>~A</td><td>~A</td></tr>~%"
	       (molecule-name-ru (first el))
	       (molecule-name-en-short (first el))
	       (second el)))
   *running-gas*)
  (format t "</table>~%~%~%"))
