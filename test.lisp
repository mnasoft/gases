;;;; test.lisp

(annot:enable-annot-syntax)



(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

(let ((sp (gethash (first '("H2O" "CH4" "N2" "O2" "Air")) *sp-db*))
      (tt 2500.d0))
  (list (molar-isobaric-heat-capacity sp tt)
	(molar-isochoric-heat-capacity sp tt)
	(molar-enthalpy sp tt)
	(molar-entropy  sp tt)
	(adiabatic-index sp tt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(apply #'+
       (mapcar #'(lambda (el)
		   (let ((elm (gethash (first el) *sp-db*)))
;;;;	      (break "~S" elm)
		     (* (sp-molar-mass elm ) (second el))))
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
		   (let ((elm (gethash (first el) *sp-db*)))
		     (* (sp-molar-mass elm ) (second el))))
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

(gethash "N2" *sp-db*)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-mass ((x molecule))
  "Возвращает молекулярную массу, [g/mol]
Пример использования
(molar-mass (gethash \"N2\" *sp-db*)) => 28.0134
(molar-mass (gethash \"CH4\" *sp-db*)) => 16.04246
"
  (molecule-mass x))

(molar-mass *air*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(let ((test (make-instance
	      'composition :components
	      (list (make-instance 'component :species (gethash "N2"              *sp-db*) :mole-fraction 0.0003 )
		    (make-instance 'component :species (gethash "CO2"             *sp-db*) :mole-fraction 0.0022 )
		    (make-instance 'component :species (gethash "CH4"             *sp-db*) :mole-fraction 0.7374 )
		    (make-instance 'component :species (gethash "C2H6"            *sp-db*) :mole-fraction 0.0593 )
		    (make-instance 'component :species (gethash "C3H8"            *sp-db*) :mole-fraction 0.1179 )
		    (make-instance 'component :species (gethash "C4H10,isobutane" *sp-db*) :mole-fraction 0.0131 )
		    (make-instance 'component :species (gethash "C4H10,n-butane"  *sp-db*) :mole-fraction 0.0379 )
		    (make-instance 'component :species (gethash "C5H12,i-pentane" *sp-db*) :mole-fraction 0.0130 )
		    (make-instance 'component :species (gethash "C5H12,n-pentane" *sp-db*) :mole-fraction 0.0139 )
		    (make-instance 'component :species (gethash "C6H14,n-hexane"  *sp-db*) :mole-fraction 0.0017 )
		    (make-instance 'component :species (gethash "C6H10,cyclo-"    *sp-db*) :mole-fraction 0.0004 )
		    (make-instance 'component :species (gethash "C6H10,cyclo-"    *sp-db*) :mole-fraction 0.0002 )
		    (make-instance 'component :species (gethash "C7H16,n-heptane" *sp-db*) :mole-fraction 0.0001 )
		    (make-instance 'component :species (gethash "C6H10,cyclo-"    *sp-db*) :mole-fraction 0.0001 )
		    (make-instance 'component :species (gethash "H2O"             *sp-db*) :mole-fraction 0.0027 )))))
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





