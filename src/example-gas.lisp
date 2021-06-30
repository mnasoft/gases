;;;; example-gas.lisp

(in-package :gases)

(defparameter *N2*
  (make-instance-composition
   `(("N2" ,(/ 100. 100)))))

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

(defparameter *pick-bel-gas*
  (make-instance-composition
   '(("CH4"	         0.9684) 
     ("C2H6"	         0.0179)
     ("C3H8"	         0.00365)
     ("C4H10,isobutane"  0.00063)
     ("C4H10,n-butane"   0.00054)
     ("CH3C(CH3)2CH3"    0.000019) ;; неопентан
     ("C5H12,i-pentane"  0.000094)
     ("C5H12,n-pentane"  0.00069)
     ("C6H14,n-hexane"   0.000119)
     ("CO2"	         0.000128)
     ("N2"	         0.0070)
     ("O2"	         0.00083))))

(defparameter *H2-40-CH4-60*
  (make-instance-composition
   '(("CH4"	         0.60) 
     ("H2"	         0.40))))

(defparameter *H2-40-CH4-60*
  (make-instance-composition
   '(#+nil
     ("CH4" 1.00)
     ("H2" 1.00))))

(molar-mass *H2-40-CH4-60*) ; => 0.0104318

(gases::adiabatic-index *H2-40-CH4-60* (+ 273.15 15.0))  ; => 1.3408859600883531d0

(defparameter *Way-Chzou*
  (make-instance-composition
   '(("CH4"  0.7647)
     ("C2H6" 0.1763)
     ("C3H8" 0.0573)
     ("C4H10,isobutane"  0.0007)
     ("C4H10,n-butane"   0.0009)
     ("C5H12,i-pentane"  0.0001)
     )))

(defparameter *Way-Chzou-1*
  (make-instance-composition
   '(("CH4"  0.6754)
     ("C2H6" 0.1105)
     ("C3H8" 0.1219)
     ("C4H10,isobutane"  0.0139)
     ("C4H10,n-butane"   0.0272)
     ("C5H12,i-pentane"  0.0057)
     ("C5H12,n-pentane"  0.0045)
     ("N2"               0.0045)
     ("CO2"              0.0364))))

(defparameter *Way-Chzou-2*
  (make-instance-composition
   '(("CH4"  0.6747)
     ("C2H6" 0.1082)
     ("C3H8" 0.1231)
     ("C4H10,isobutane"  0.0141)
     ("C4H10,n-butane"   0.0279)
     ("C5H12,i-pentane"  0.0072)
     ("C5H12,n-pentane"  0.0061)
     ("C6H14,n-hexane"   0.0007)
     ("N2"               0.0026)
     ("CO2"              0.0354))))

(defparameter *Way-Chzou-3*
  (make-instance-composition
   '(("CH4"  0.8785)
     ("C2H6" 0.0852)
     ("C3H8" 0.0138)
     ("C4H10,isobutane"  0.0008)
     ("C4H10,n-butane"   0.0005)
     ("CO2"              0.0212))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gases:molar-mass                   *Way-Chzou*  ) ; => 20.1957304485625d0
(gases:adiabatic-index              *Way-Chzou* (+ 273.15 15)) ; => 1.2596807623575537d0 (125.96807623575536d0%)
(gases:Q-work-low                   *Way-Chzou*)
(gases:relativ-air-mass-for-burning *Way-Chzou*)

(gases:molar-mass                   *Way-Chzou-1*  )  ; => 24.38570891141071d0
(gases:adiabatic-index              *Way-Chzou-1* (+ 273.15 50)) ; => 1.21145303407712d0 (121.14530340771199d0%)
(gases:Q-work-low                   *Way-Chzou-1*) ; => -44755.68000335859d0
(gases:relativ-air-mass-for-burning *Way-Chzou-1*)

(gases:molar-mass                   *Way-Chzou-2*  )  ; => 24.597293909120197d0
(gases:adiabatic-index              *Way-Chzou-2* (+ 273.15 50)) 
(gases:Q-work-low                   *Way-Chzou-2*) ; => -44933.63119750429d0
(gases:relativ-air-mass-for-burning *Way-Chzou-2*)

(gases:molar-mass                   *Way-Chzou-3*)   ; => 18.2722627596264d0
(gases:adiabatic-index              *Way-Chzou-3* (+ 273.15 15))  ; => 1.2868344212928924d0 (128.68344212928923d0%)
(gases:Q-work-low                   *Way-Chzou-3*)   ; => -46979.08236386708d0
(gases:relativ-air-mass-for-burning *Way-Chzou-3*) 
