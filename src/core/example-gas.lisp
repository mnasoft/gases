;;;; example-gas.lisp

(in-package gases/core)

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

(defparameter *CASE-1*
  (make-instance-composition
   '(("N2"               0.0160)
     ("CO2"              0.0120)
     ("CH4"              0.8150)
     ("C2H6"             0.0690)
     ("C3H8"             0.0470)
     ("C4H10,isobutane"  0.0110)
     ("C4H10,n-butane"   0.0140)
     ("C5H12,i-pentane"  0.0050)
     ("C5H12,n-pentane"  0.0030)
     ("C6H14,n-hexane"   0.0030)
     ("C7H16,n-heptane"  0.0025)
     ("H2O"              0.0025)
     )))

(defparameter *CASE-2*
  (make-instance-composition
   '(("N2"               0.0010)
     ("CO2"              0.0207)
     ("CH4"              0.8423)
     ("C2H6"             0.0728)
     ("C3H8"             0.0372)
     ("C4H10,isobutane"  0.0067)
     ("C4H10,n-butane"   0.0104)
     ("C5H12,i-pentane"  0.0031)
     ("C5H12,n-pentane"  0.0027)
     ("C6H14,n-hexane"   0.0024)
     ("C7H16,n-heptane"  0.0001)
     ("H2O"              0.0006)
     )))

(defparameter *CASE-3*
  (make-instance-composition
   '(("N2"               0.0190)
     ;;("CO2"              0.0000)
     ("CH4"              0.9770)
     ("C2H6"             0.0040)
     ;;("C3H8"             0.0000)
     ;;("C4H10,isobutane"  0.0000)
     ;;("C4H10,n-butane"   0.0000)
     ;;("C5H12,i-pentane"  0.0000)
     ;;("C5H12,n-pentane"  0.0000)
     ;;("C6H14,n-hexane"   0.0000)
     ;;("C7H16,n-heptane"  0.0000)
     ;;("H2O"              0.0000)
     )))

(defparameter *CASE-4*
  (make-instance-composition
   '(("N2"               0.0010)
     ;;("CO2"              0.0000)
     ("CH4"              0.9950)
     ("C2H6"             0.0040)
     ;; ("C3H8"             0.0000)
     ;; ("C4H10,isobutane"  0.0000)
     ;; ("C4H10,n-butane"   0.0000)
     ;; ("C5H12,i-pentane"  0.0000)
     ;; ("C5H12,n-pentane"  0.0000)
     ;; ("C6H14,n-hexane"   0.0000)
     ;; ("C7H16,n-heptane"  0.0000)
     ;; ("H2O"              0.0000)
     )))

(defparameter *CASE-5*
  (make-instance-composition
   '(("N2"               0.0170)
     ("CO2"              0.0130)
     ("CH4"              0.8930)
     ("C2H6"             0.0750)
     ("C3H8"             0.0020)
     ;; ("C4H10,isobutane"  0.0000)
     ;; ("C4H10,n-butane"   0.0000)
     ;; ("C5H12,i-pentane"  0.0000)
     ;; ("C5H12,n-pentane"  0.0000)
     ;; ("C6H14,n-hexane"   0.0000)
     ;; ("C7H16,n-heptane"  0.0000)
     ;; ("H2O"              0.0000)
     )))

"
| Компонент         | CASE-1 | CASE - 2 | CASE - 3 | CASE-4 | CASE-5 |
|                   |  Мол % |    Мол % |    Мол % |        |  Мол % |
| Азот              |   1.60 |     0.10 |     1.90 |    0.1 |    1.7 |
| Двуокись углерода |   1.20 |     2.07 |     0.00 |      0 |    1.3 |
| Сульфид водорода  |   0.00 |     0.00 |     0.00 |      0 |      0 |
| Метан             |  81.50 |    84.23 |    97.70 |   99.5 |   89.3 |
| Этан              |   6.90 |     7.28 |     0.40 |    0.4 |    7.5 |
| Пропан            |   4.70 |     3.72 |     0.00 |      0 |    0.2 |
| l-бутан           |   1.10 |     0.67 |     0.00 |      0 |      0 |
| N-бутан           |   1.40 |     1.04 |     0.00 |      0 |      0 |
| l-пентан          |   0.50 |     0.31 |     0.00 |      0 |      0 |
| N-пентан          |   0.30 |     0.27 |     0.00 |      0 |      0 |
| N-гексан          |   0.30 |     0.24 |     0.00 |      0 |      0 |
| N-гептан+         |   0.25 |     0.01 |     0.00 |      0 |      0 |
| Вода              |   0.25 |     0.06 |     0.00 |      0 |      0 |
| Всего             | 100.00 |   100.00 |   100.00 |    100 |    100 |
"

(gases/reac:q-work-low *CASE-1*) ; => -46389.00241685833d0
(gases/reac:wobber-low *CASE-1*) ; => -50778.84832399529d0
(gases/core:density *CASE-1* 101325.0 273.0)  ; => 0.9277216319527446d0 (92.77216319527446d0%)

(gases/reac:q-work-low *CASE-2*) ; => -46690.605455714656d0
(gases/reac:wobber-low *CASE-2*) ; => -50038.40995470803d0
(gases/core:density *CASE-2* 101325.0 273.0) ; => 0.889262639526845d0 (88.92626395268451d0%)

(gases/reac:q-work-low *CASE-3*) ; => -48377.86197467065d0
(gases/reac:wobber-low *CASE-3*) ; => -46935.95050879597d0
(gases/core:density *CASE-3* 101325.0 273.0) ; => 0.7287858780495047d0 (72.87858780495047d0%)

(gases/reac:q-work-low *CASE-4*) ; => -49921.59868862762d0
(gases/reac:wobber-low *CASE-4*) ; => -48112.99139507732d0
(gases/core:density *CASE-4* 101325.0 273.0) ; => 0.7191670915213143d0 (71.91670915213143d0%)

(gases/reac:q-work-low *CASE-5*) ; => -46728.72809775131d0
(gases/reac:wobber-low *CASE-5*) ; => -47228.671437333716d0
(gases/core:density *CASE-1* 101325.0 273.0) ; => 0.9277216319527446d0 (92.77216319527446d0%)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gases/reac:molar-mass                   *Way-Chzou*) ; => 20.1957304485625d0
(gases/core::adiabatic-index             *Way-Chzou* (+ 273.15 15))  ; => 1.2596807623575537d0 (125.96807623575536d0%)

(gases/reac:q-work-low                   *Way-Chzou*) ; => -48883.30788096489d0
(gases/reac:relativ-air-mass-for-burning *Way-Chzou*)  ; => 16.734367091613393d0

(gases/reac:molar-mass                   *Way-Chzou-1*  )  ; => 24.38570891141071d0
(gases/core::adiabatic-index             *Way-Chzou-1* (+ 273.15 50))  ; => 1.21145303407712d0 
                                        ; => 1.21145303407712d0 (121.14530340771199d0%)
(gases/reac:q-work-low                   *Way-Chzou-1*)  ; => -44755.68000335859d0
#+nil
(gases/reac:relativ-air-mass-for-burning *Way-Chzou-1*) 

(gases/reac:molar-mass                   *Way-Chzou-2*  )  ; => 24.597293909120197d0
(gases/core::adiabatic-index             *Way-Chzou-2* (+ 273.15 50)) 
(gases/reac::q-work-low                  *Way-Chzou-2*) ; => -44933.63119750429d0
#+nil
(gases/reac:relativ-air-mass-for-burning *Way-Chzou-2*)

(gases/reac:molar-mass                   *Way-Chzou-3*)   ; => 18.2722627596264d0
(gases/core::adiabatic-inde              *Way-Chzou-3* (+ 273.15 15))  ; => 1.2868344212928924d0 (128.68344212928923d0%)
(gases/reac:q-work-low                   *Way-Chzou-3*)   ; => -46979.08236386708d0
#+nil
(gases/reac:relativ-air-mass-for-burning *Way-Chzou-3*) 

(gases/reac:relativ-air-mass-for-burning *H2*)
(gases/reac:q-work-low *H2*) ; => -119960.51352263031d0
(gases/reac:relativ-air-mass-for-burning *H2*)
(gases/core::adiabatic-index             *H2* (+ 273.15 15)) ; => 1.4066457489295208d0 
(gases/reac:combustion-reaction          *H2*)
