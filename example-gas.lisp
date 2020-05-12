;;;; example-gas.lisp

(in-package :gases)

(defparameter *Air*
  (make-instance-composition
   `(("N2" ,(/ 78.084 100))
     ("O2" ,(/ 20.9476 100))
     ("Ar" ,(/ .9365 100))
     ("CO2",(/ .0319 100)))))

(defparameter *N2*
  (make-instance-composition
   `(("N2" ,(/ 100. 100)))))

(defparameter *O2*
  (make-instance-composition
   `(("O2" ,(/ 100. 100)))))

(defparameter *H2*
  (make-instance-composition
   `(("H2" ,(/ 100. 100)))))

(defparameter *Ar*
  (make-instance-composition
   `(("Ar" ,(/ 100. 100)))))

(defparameter *CO2*
  (make-instance-composition
   `(("CO2" ,(/ 100. 100)))))

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
     ("C6H12,cyclo-"    0.0003)     
     ("C7H16,n-heptane" 0.0001)
     ("H2O"             0.0025))))

(defparameter *stopping-gas*
  (make-instance-composition
   '(( "N2"	          0.0002)
     ( "CO2"	          0.0030)
     ( "CH4"	          0.4042)
     ( "C2H6"	          0.0658)
     ( "C3H8"	          0.2469)
     ( "C4H10,isobutane"  0.0506)
     ( "C4H10,n-butane"   0.1621)
     ( "C5H12,i-pentane"  0.0359)
     ( "C5H12,n-pentane"  0.0277)
     ( "C6H14,n-hexane"   0.0009)
     ( "C5H10,cyclo-"     0.0002)
     ( "C6H12,cyclo-"     0.0001)
     ( "C7H16,n-heptane"  0.0000)
     ( "H2O"	          0.0024))))
