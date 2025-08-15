;;;; test.lisp

(in-package :gases/core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gas*
  (gases/core:make-instance-composition
   '(
     ("CO2"             0.1088 )
     ("N2"              0.0095 )
     ("H2O"             0.0021 )
     ("CH4"             0.6726 ) ; 0.6724 
     ("C2H6"            0.1020 )
     ("C3H8"            0.0478 )
     ("C4H10,isobutane" 0.0116 )
     ("C4H10,n-butane"  0.0214 )
     ("C5H12,i-pentane" 0.0094 )
     ("C5H12,n-pentane" 0.0083 )
     ("C6H14,n-hexane"  0.0057 )
     ("C7H16,n-heptane" 0.0008 ))))

(gases/reac:relativ-air-mass-for-burning *gas*)

(gases/reac:relativ-air-mass-for-burning
 (make-instance-composition
  '(("CH4" 0.0) ("H2" 1.0))))

(gases/reac:relativ-oxigen-mass-for-burning
 (make-instance-composition
  '(("CH4" 0.0) ("H2" 1.0))))

(molar-mass
 (make-instance-composition
  '(("CH4" 1.0) ("H2" 0.0))))

(density
 (make-instance-component "CH4" 1.0) 101325.0 273.)

(density
 (make-instance-composition
  '(("CH4" 0.8) ("H2" 0.2)))
 101325.0 273.)

(density
 (make-instance-composition
  '(("CH4" 0.9) ("H2" 0.1)))
 101325.0 273.)

(density
 (make-instance-composition
  '(("CH4" 0.95) ("H2" 0.05)))
 101325.0 273.)


(density (get-sp "H2") +p-normal+   +c-0+ )
(density (get-sp "CH4") +p-normal+  293.15)

(gases/reac:combustion-reaction (get-sp "C2H4"))


(gases/reac:q-work-low (get-sp "H2"))
(gases/reac:q-work-low (get-sp "CH4"))

(gases/reac:q-work-low (make-instance-component "H2" 0.030457929595689225d0  :mass))
(gases/reac:q-work-low (make-instance-composition '(("H2" 0.2) ("CH4" 0.80))))

(wobber-low (make-instance-composition '(("H2" 0.2) ("CH4" 0.80))))

(density (make-instance-composition '(("H2" 0.2) ("CH4" 0.80))) 101325.0 273.15 )
'(("H2" 0.2) ("CH4" 0.80))

(molar-mass (make-instance-composition '(("H2" 0.2) ("CH4" 0.80))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *c-n* (make-instance-composition '(("H2" 0.95) ("CH4" 0.05))))
(type-of *c-n*)

(gases/reac:relativ-air-mass-for-burning *c-n*)
(gases/reac:thermal-effect *c-n* )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select (&key atoms designation description)
  (when atoms
    (maphash
     #'(lambda (key value)
	 )
     (get-db))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(get-sp "NaOH")
(get-sp "H2O")
(get-sp	"NaCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (remove-method #'ADAPT-MOLE-FRACTIONS (find-method #'ADAPT-MOLE-FRACTIONS '() (mapcar #'find-class '(t t))))

(with-open-file (fl "/home/namatv/quicklisp/local-projects/clisp/gases/data/termo.inp"
		    :direction :output :if-exists :supersede)
  (dump (get-db) fl))

(relativ-air-mass-for-burning (make-instance-composition '(("CH4" 0.1) ("C6H6" 0.9)) :mass))


(defparameter *sp* (get-sp "CH4"))

(gases/db:dump (get-sp "NH3") t )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(gases/db::check-sp (get-sp "N2PO"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *rt*  
  (make-instance 'gases/reac:<reactant> :species (get-sp "H2O") :mole 2))

(type-of *rt*)
(molar-mass *rt*)

(gases/reac:thermal-effect *rt*)

(<sp>-heat-formation (species *rt*))



(defparameter *reac*
  (make-instance 'gases/reac:<reaction> :reactant-names '("C2H5OH" "O2") :product-names '("H2O" "CO2")))

(defparameter *reac*
  (make-instance 'gases/reac:<reaction> :reactant-names '("C2H5OH" "O2") :product-names '("H2O(L)" "CO2")))

(defparameter *reac*
  (make-instance 'gases/reac:<reaction> :reactant-names '("H2" "O2") :product-names '("H2O")))

(defparameter *reac*
  (make-instance 'gases/reac:<reaction> :reactant-names '("CH4" "O2") :product-names '("H2O(L)" "CO2")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((sp (get-sp (first '("H2O" "CH4" "N2" "O2" "Air")) ))
      (tt 2500.d0))
  (list (molar-isobaric-heat-capacity sp tt)
	(molar-isochoric-heat-capacity sp tt)
	(molar-enthalpy sp tt)
	(molar-entropy  sp tt)
	(adiabatic-index sp tt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(apply #'+
       (mapcar #'(lambda (el)
		   (let ((elm (get-sp (first el) )))
;;;;	      (break "~S" elm)
		     (* (<sp>-molar-mass elm ) (second el))))
	       '(("N2"	                0.0003)
		 ("CO2"	                0.0022)
		 ("CH4"	                0.7374 "C1")
		 ("C2H6"	        0.0593)
		 ("C3H8"	        0.1179)
		 ("C4H10,isobutane" 	0.0131)
		 ("C4H10,n-butane"	0.0379)
		 ("C5H12,i-pentane" 	0.0130)
		 ("C5H12,n-pentane"	0.0139)
		 ("C6H14,n-hexane" 	0.0017)
		 ("C6H12,1-hexene"      0.0004 "Mcyclo_C5")
		 ("C6H12,cyclo-"	0.0002)
		 ("C7H16,n-heptane"     0.0001)
		 ("C7H14,1-heptene" 	0.0001 "Mcyclo_C6")
		 ("H2O"	                0.0025))))

(apply #'+
       (mapcar #'(lambda (el)
		   (let ((elm (get-sp (first el) )))
		     (* (<sp>-molar-mass elm ) (second el))))
	       '(
		 ("CO2"	                0.0739)
		 ("N2"	                0.0004)
		 ("CH4"	                0.8134 "C1")
		 ("C2H6"	        0.0558)
		 ("C3H8"	        0.0367)
		 ("C4H10,isobutane" 	0.0061)
		 ("C4H10,n-butane"	0.0087)
		 ("C5H12,i-pentane" 	0.0023)
		 ("C5H12,n-pentane"	0.0016)
		 ("C6H14,n-hexane" 	0.0006)
;;;;		 ("C6H12,1-hexene"      0.0004 "Mcyclo_C5")
;;;;		 ("C6H12,cyclo-"	0.0002)
		 ("C7H16,n-heptane"     0.0005)
;;;;		 ("C7H14,1-heptene" 	0.0001 "Mcyclo_C6")
;;;;		 ("H2O"	                0.0025)
		 )))

(get-sp "N2" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-mass ((x <molecule>))
  "Возвращает молекулярную массу, [g/mol]
Пример использования
(molar-mass (get-sp \"N2\" )) => 28.0134
(molar-mass (get-sp \"CH4\" )) => 16.04246
"
  (molecule-mass x))

(molar-mass *air*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(let ((test (make-instance
	      'composition :components
	      (list (make-instance 'component :species (get-sp "N2"              ) :mole-fraction 0.0003 )
		    (make-instance 'component :species (get-sp "CO2"             ) :mole-fraction 0.0022 )
		    (make-instance 'component :species (get-sp "CH4"             ) :mole-fraction 0.7374 )
		    (make-instance 'component :species (get-sp "C2H6"            ) :mole-fraction 0.0593 )
		    (make-instance 'component :species (get-sp "C3H8"            ) :mole-fraction 0.1179 )
		    (make-instance 'component :species (get-sp "C4H10,isobutane" ) :mole-fraction 0.0131 )
		    (make-instance 'component :species (get-sp "C4H10,n-butane"  ) :mole-fraction 0.0379 )
		    (make-instance 'component :species (get-sp "C5H12,i-pentane" ) :mole-fraction 0.0130 )
		    (make-instance 'component :species (get-sp "C5H12,n-pentane" ) :mole-fraction 0.0139 )
		    (make-instance 'component :species (get-sp "C6H14,n-hexane"  ) :mole-fraction 0.0017 )
		    (make-instance 'component :species (get-sp "C6H10,cyclo-"    ) :mole-fraction 0.0004 )
		    (make-instance 'component :species (get-sp "C6H10,cyclo-"    ) :mole-fraction 0.0002 )
		    (make-instance 'component :species (get-sp "C7H16,n-heptane" ) :mole-fraction 0.0001 )
		    (make-instance 'component :species (get-sp "C6H10,cyclo-"    ) :mole-fraction 0.0001 )
		    (make-instance 'component :species (get-sp "H2O"             ) :mole-fraction 0.0027 )))))
  (adiabatic-index test 473))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;T 220      230       240     250      260      270       280      290
;P 2.2      2.2       2.2     2.2      2.2      2.2       2.2      2.2
;H -4818.52 -4797.44 -4776.26 -4754.96 -4733.53 -4711.94 -4690.18 -4668.22
;U -4932.54 -4916.64 -4900.64 -4884.53 -4868.28 -4851.88 -4835.30 -4818.52
;G -7140.57 -7246.59 -7353.53 -7461.36 -7570.03 -7679.54 -7789.85 -7900.94
;S  10.5548  10.6885  10.7387  10.8256  10.9096  10.9911  11.0703  11.1473


;T 220      230       240     250      260      270       280      290
;P 26.      26.       26.     26.      26.      26.       26.      26.
;H -4818.52 -4797.44 -4776.26 -4754.96 -4733.53 -4711.94 -4690.18 -4668.22
;U -4932.54 -4916.64 -4900.64 -4884.53 -4868.28 -4851.88 -4835.30 -4818.52
;G -6858.98 -6952.20 -7046.34 -7141.36 -7237.24 -7333.95 -7431.46 -7529.75
;S  9.27488   9.3685   9.4587   9.5456   9.6297   9.7111   9.7903   9.8673





(gases/core:make-instance-composition  
		`(("N2" ,(/ 76.8 100.0))
		  ("O2" ,(/ 23.2 100.0)))
                :mass)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  
  (defparameter *G-Ο2* 0.232d0)  
  (defparameter *G-N2* 0.768d0)

  (defparameter *mu-CH4* 0.016d0)
  (defparameter *mu-O2* 0.032d0)
  (defparameter *mu-N2* 0.028d0)
  
  (defparameter *G-CH4* (* (+ 0.012552d0 0.092048d0) 2.0)
    "Расход метана, поступающего в 1/8 ЖТ КС, кг/с")  ; => *G-CH4* 0.2092d0 (20.919999999999998d0%)

  (defparameter *G-ΟK* (- (+ 5.3190d0 5.0962d0) (+ 0.0415d0 0.0418d0))
    "Расход окислителя, поступающего в 1/8 ЖТ КС, кг/с") ; => *G-ΟK* 10.3319d0
    

  (defparameter *X-CH4* (/ *G-CH4* *mu-CH4*)
    "Расход метана, подаваемый в 1/8 ЖТ КС, моль/с") ; => *XT-1/8* 13.074999

  (defparameter *G-O2-ВХ* (/ (* *G-Ο2* *G-ΟK*) (+ *G-CH4* *G-ΟK*))
    "Доля кислорода на входе с ЖТ КС")  ; => *G-O2-ВХ* 0.22739568

  (defparameter *G-N2-ВХ* (/ (* *G-N2* *G-ΟK*) (+ *G-CH4* *G-ΟK*))
    "Доля азота на входе с ЖТ КС.") ; => *G-N2-ВХ* 0.75275815 (75.27582%)

  (defparameter *G-CH4-ВХ* (/ *G-CH4* (+ *G-CH4* *G-ΟK*))
    "Доля азота на входе с ЖТ КС.") ; => *G-CH4-ВХ* 0.019846126788112435d0 

  (defparameter *mu-OK* (/ (+ (/ *G-Ο2* *mu-O2*) (/ *G-N2* *mu-N2*)))) ; => MU-OK 0.028836252 (2.8836253%)

  (defparameter *X-O2* (/ (* *G-ΟK* *G-Ο2*)  *mu-OK*)) ; => 83.12455 моль/с O2

  (defparameter *X-N2* (/ (* *G-ΟK* *G-N2*)  *mu-OK*)) ; => 275.17096 моль/с N2
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *reac*
  (make-instance 'gases/reac:<reaction> :reactant-names '("H2" "O2") :product-names '("H2O")))

(gases/reac:thermal-effect *reac*)

(+
 (* 2.0 (molar-enthalpy (get-sp "H2") 298.15))
 (molar-enthalpy (get-sp "O2") (+ 298.15 400.0)))
;=> 12439.059

(<sp>-heat-formation (get-sp "O2"))

(<sp>-heat-formation (get-sp "H2O"))
;=> (+ 12439.058830211387d0 -483652.0d0)   ; =>

(gases/reac:thermal-effect *reac*) ;  (/ -483652.0 2)  -241826.0 "J"
(gases/reac:q-work-low (get-sp "H2"))

(/ -471212.9411697886d0 2) 

(temperature-by-molar-enthalpy (get-sp "H2O") -126965.5633264229d0)

(molar-enthalpy (get-sp "H2O") (+ 2500 273.15)) ; => -126965.5633264229d0, "J/mol"

(molar-enthalpy (get-sp "H2O") 298.15) ; -241824.623 "J/mol"

(molar-enthalpy (get-sp "H2O")  (+ 298.15    0.0))  ; => -241824.62351031243d0, "J/mol"
(molar-enthalpy (get-sp "H2O")  (+ 298.15  400.0))  ; => -227702.0265908651d0, "J/mol"

(gases/reac:<reaction>-reactants *reac*)
(car (gases/reac:<reaction>-products *reac*))

(culc-koeffitients *reac*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; 2*H2 + 1*O2 => 2*H2O
;;;; 2*0.2091*H2 + 0.2091*O2 + 0.7909*N2 => 2*0.2091*H2O + 0.7909*N2

(/ 0.4182 (+ 0.4182 0.7909)) ; => 0.34587708 (34.587708%)
(/ 0.7909 (+ 0.4182 0.7909)) ; => 0.65412292 (65.412292%)

(defparameter *reac* 
  (make-instance 'gases/reac:<reaction>
                 :reactant-names '("H2" "O2")
                 :product-names '("H2O")))

*reac*  

(gases/reac:thermal-effect *reac*)  ; => -483652.0d0, "J"

(* -1 0.34587708 1/2 (gases/reac:thermal-effect *reac*))  ; => 83642.07102757692d0

(defparameter *H2+AIR+product*
  (make-instance-composition
   `(("H2O" 0.34587708)
     ("N2"  0.65412292))))

(molar-enthalpy *H2+AIR+product* 273.15)   ; => -84407.78859927837d0, "J/mol"

(- (temperature-by-molar-enthalpy
    *H2+AIR+product*
    (+ (molar-enthalpy *H2+AIR+product* 273.15)
       (* -1
          0.34587708 1/2
          (gases/reac:thermal-effect *reac*))))
   273.15) ; => 2221.8362


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 1*CH4 + 2*O2 => 2*H2O + 1*CO2
;;;; 0.5*0.2091*CH4 + 0.2091*O2 + 0.7909*N2 => 0.2091*H2O + 0.5*0.2091*CH4  + 0.7909*N2

(defparameter *reac*
  (make-instance 'gases/reac:<reaction>
                 :reactant-names '("CH4" "O2")
                 :product-names  '("H2O" "CO2")))

*reac*  ; => 1*CH4 + 2*O2 => 2*H2O + 1*CO2

(defparameter *reac* 
  (make-instance 'gases/reac:<reaction>
                 :reactants
                 (list 
                  (make-instance 'gases/reac:<reactant>
                                 :species (get-sp "CH4")
                                 :mole (* 0.5 0.2091))
                  (make-instance 'gases/reac:<reactant>
                                 :species (get-sp "O2")
                                 :mole (* 1.0 0.2091))
                  (make-instance 'gases/reac:<reactant>
                                 :species (get-sp "N2")
                                 :mole (* 1.0 0.7909)))
                 :products
                 (list 
                  (make-instance 'gases/reac:<product>
                                 :species (get-sp "H2O")
                                 :mole (* 1.0 0.2091))
                  (make-instance 'gases/reac:<product>
                                 :species (get-sp "CH4")
                                 :mole (* 0.5 0.2091))
                  (make-instance 'gases/reac:<reactant>
                                 :species (get-sp "N2")
                                 :mole (* 1.0 0.7909)))))

(molar-enthalpy (gases/reac:<reaction>-reactants *reac*) 2298.15) ; => -7799.385660305199d0
(molar-enthalpy (gases/reac:<reaction>-products  *reac*) 298.15) ; => -58364.91254359074d0




(gases/reac:thermal-effect *reac*) ; => -802562.0d0, "J/mol"

(temperature-by-molar-enthalpy
 (gases/reac:<reaction>-products *reac*)
 (- (+ (molar-enthalpy (gases/reac:<reaction>-products  *reac*) 298.15)
       (- (molar-enthalpy (gases/reac:<reaction>-reactants *reac*) (+ 298.15 500.0))
          (molar-enthalpy (gases/reac:<reaction>-reactants *reac*) (+ 298.15 0.0)))
       )
    (gases/reac:thermal-effect *reac*)))


(<sp>-heat-formation (get-sp "CH4"))

