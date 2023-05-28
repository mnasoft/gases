(defpackage :gases/tests
  (:use #:cl #:fiveam)
  (:export :run!
	   :all-tests
	   :test-geses))

(in-package :gases/tests)

(def-suite all-tests
  :description "Мастер-набор всех тестов проекта gases.")

(in-suite all-tests)

(defun test-gases ()
  (run! 'all-tests))

;;;; (test-gases)

(def-fixture fix-gases ()
  (let ((+Air+ (gases/core:make-instance-composition
		`(("N2" ,(/ 78.084 100))
		  ("O2" ,(/ 20.9476 100))
		  ("Ar" ,(/ .9365 100))
		  ("CO2",(/ .0319 100)))))

	(+N2+ (gases/reac:make-instance-composition
	       `(("N2" ,(/ 100. 100)))))

	(+O2+ (gases/core:make-instance-composition
	   `(("O2" ,(/ 100. 100)))))

	(+H2+ (gases/core:make-instance-composition
	   `(("H2" ,(/ 100. 100)))))

	(+Ar+ (gases/core:make-instance-composition
	   `(("Ar" ,(/ 100. 100)))))

	(+CO2+ (gases/core:make-instance-composition
	   `(("CO2" ,(/ 100. 100)))))

	(+running-gas+ (gases/core:make-instance-composition
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

	(+stopping-gas+ (gases/core:make-instance-composition
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
	     ( "H2O"	          0.0024)))))
    (&body)))

(def-test component-mole-fraction-test ()
  "Проверка молярного состава."
  (with-fixture fix-gases ()
    (is-true (math/core:semi-equal
	      (gases/core:mole-fraction
	       (gethash "N2"
			(gases/core:<composition>-components +air+)))
	      0.780840))
    (is-true (math/core:semi-equal
	      (gases/core:mole-fraction
	       (gethash "O2"
			(gases/core:<composition>-components +air+)))
	      0.209476))
    (is-true (math/core:semi-equal
	      (gases/core:mole-fraction
	       (gethash "CO2"
			(gases/core:<composition>-components +air+)))
	      0.000319))
    (is-true (math/core:semi-equal
	      (gases/core:mole-fraction
	       (gethash "Ar"
			(gases/core:<composition>-components +air+)))
	      0.009365))))

(def-test molar-fraction-summ-test ()
  (with-fixture fix-gases ()
    (is-true (math/core:semi-equal (gases/core:molar-fraction-summ +air+) 1.0))
    (is-true (math/core:semi-equal (gases/core:molar-mass +air+)  28.965115))
    (gases/core:culc-mass-fractions +air+)
    (is-true (math/core:semi-equal (gases/core:mass-fraction-summ +air+)  1.0))
    (is-true (gases/core:check-mole-fraction +air+))
    (is-true (gases/core:check-mass-fraction +air+))))


(def-test mix-composition-test ()
  (with-fixture fix-gases ()
    (let ((cmp (multiple-value-bind (cmp-1 mfr-1)
		   (gases/core:mix-composition
		    +N2+ 0.7551837
		    +O2+ 0.23141564)
		 (multiple-value-bind (cmp-2 mfr-2)
		     (gases/core:mix-composition
		      +Ar+  0.012915987
		      +CO2+ 4.846875e-4)
		   (gases/core:mix-composition cmp-1 mfr-1 cmp-2 mfr-2)))))
      (is-true (math/core:semi-equal (gases/core:mole-fraction (gases/core:reference "N2" +air+))
				(gases/core:mole-fraction (gases/core:reference "N2" cmp)))))))

(def-test combustion-reaction-test ()
  (is-true (eq 'gases/reac:<reaction>
	       (type-of
		(gases/reac:combustion-reaction (gases/db:get-sp "H2"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite reaction-tests
  :description "Проверка составления химических уравнений ."
  :in all-tests)

(in-suite reaction-tests)

(def-fixture fix-wolfram-species ()
  (setf (gases/db:get-sp "Na2WO4")
	(make-instance 'gases/db:<sp>
		       :name "Na2WO4"
		       :chemical-formula '(("NA" 2.0) ("W" 1.0) ("O" 4.0) ("" 0.0) ("" 0.0))))

  (setf (gases/db:get-sp "WF6")
	(make-instance 'gases/db:<sp>
		       :name "WF6"
		       :chemical-formula '(("W" 1.0) ("F" 6.0) ("" 0.0) ("" 0.0) ("" 0.0))))

  (setf  (gases/db:get-sp "WOF4")
	 (make-instance 'gases/db:<sp>
			:name "WOF4"
			:chemical-formula '(("W" 1.0) ("O" 1.0) ("F" 4.0) ("" 0.0) ("" 0.0))))
  (&body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-test 2*W+4*HNO3+10*HF=>1*WF6+1*WOF4+4*NO+7*H2O-test ()
  "  2*W + 4*HNO3 + 10*HF => 1*WF6 + 1*WOF4 + 4*NO + 7*H2O  "
  (with-fixture fix-wolfram-species ()
    (let ((reac (make-instance 'gases/reac:<reaction>
			       :reactant-names (list "W" "HNO3" "HF")
			       :product-names(list "WF6" "WOF4" "NO" "H2O"))))
      (is-true
       (and
	(= 2  (gases/reac:moles-number (first  (gases/reac:<reaction>-reactants reac))))
	(= 4  (gases/reac:moles-number (second (gases/reac:<reaction>-reactants reac))))
	(= 10 (gases/reac:moles-number (third  (gases/reac:<reaction>-reactants reac))))
	(= 1  (gases/reac:moles-number (first  (gases/reac:<reaction>-products  reac))))
	(= 1  (gases/reac:moles-number (second (gases/reac:<reaction>-products  reac))))
	(= 4  (gases/reac:moles-number (third  (gases/reac:<reaction>-products  reac))))
	(= 7  (gases/reac:moles-number (fourth (gases/reac:<reaction>-products  reac)))))))))

(def-test 2*W+4*NaOH+3*O2=>2*Na2WO4+2*H2O-test ()
  "  2*W + 4*NaOH + 3*O2 => 2*Na2WO4 + 2*H2O  "
  (with-fixture fix-wolfram-species ()
    (let ((reac (make-instance 'gases/reac:<reaction>
			       :reactant-names (list "W" "NaOH" "O2")
			       :product-names (list "Na2WO4" "H2O"))))
      (is-true
       (and
	(= 2  (gases/reac:moles-number (first  (gases/reac:<reaction>-reactants reac))))
	(= 4  (gases/reac:moles-number (second (gases/reac:<reaction>-reactants reac))))
	(= 3  (gases/reac:moles-number (third  (gases/reac:<reaction>-reactants reac))))
	(= 2  (gases/reac:moles-number (first  (gases/reac:<reaction>-products  reac))))
	(= 2  (gases/reac:moles-number (second (gases/reac:<reaction>-products  reac)))))))))

(def-test 1*W+2*NaOH+3*NaNO3=>1*Na2WO4+3*NaNO2+1*H2O-test ()
  "  1*W + 2*NaOH + 3*NaNO3 => 1*Na2WO4 + 3*NaNO2 + 1*H2O  "
  (with-fixture fix-wolfram-species ()
    (let ((reac (make-instance 'gases/reac:<reaction>
			       :reactant-names (list "W" "NaOH" "NaNO3")
			       :product-names  (list "Na2WO4" "NaNO2" "H2O"))))
      (is-true
       (and
	(= 1  (gases/reac:moles-number (first  (gases/reac:<reaction>-reactants reac))))
	(= 2  (gases/reac:moles-number (second (gases/reac:<reaction>-reactants reac))))
	(= 3  (gases/reac:moles-number (third  (gases/reac:<reaction>-reactants reac))))
	(= 1  (gases/reac:moles-number (first  (gases/reac:<reaction>-products  reac))))
	(= 3  (gases/reac:moles-number (second (gases/reac:<reaction>-products  reac))))
	(= 1  (gases/reac:moles-number (third  (gases/reac:<reaction>-products  reac)))))))))

(def-test 2*H2+1*O2=>2*H2O-test ()
  "  2*H2 + 1*O2 => 2*H2O  "
  (with-fixture fix-wolfram-species ()
    (let ((reac (make-instance 'gases/reac:<reaction>
			       :reactant-names (list "H2" "O2")
			       :product-names  (list "H2O"))))
      (is-true
       (and
	(= 2  (gases/reac:moles-number (first  (gases/reac:<reaction>-reactants reac))))
	(= 1  (gases/reac:moles-number (second (gases/reac:<reaction>-reactants reac))))
	(= 2  (gases/reac:moles-number (first  (gases/reac:<reaction>-products  reac)))))))))

(def-test 2*KMnO4+H2O2+3*H2SO4=>3*O2+4*H2O+2*MnSO4+K2SO4-test ()
  " 2*KMnO4+H2O2+3*H2SO4=>3*O2+4*H2O+2*MnSO4+K2SO4  "
  (setf (gases/db:get-sp "KMnO4")
	(make-instance 'gases/db:<sp>
		       :name "KMnO4"
		       :chemical-formula '(("K" 1.0) ("MN" 1.0) ("O" 4.0) ("" 0.0) ("" 0.0))))

  (setf (gases/db:get-sp "MnSO4")
	(make-instance 'gases/db:<sp>
		       :name "MnSO4"
		       :chemical-formula '(("MN" 1.0) ("S" 1.0) ("O" 4.0) ("" 0.0) ("" 0.0))))
  (let ((reac (make-instance 'gases/reac:<reaction>
			     :reactant-names (list "KMnO4" "H2O2" "H2SO4")
			     :product-names  (list  "O2"  "H2O" "MnSO4" "K2SO4" ))))
    (is-true
       (and
	(= 2  (gases/reac:moles-number (first  (gases/reac:<reaction>-reactants reac))))
	(= 1  (gases/reac:moles-number (second (gases/reac:<reaction>-reactants reac))))
	(= 3  (gases/reac:moles-number (third  (gases/reac:<reaction>-reactants reac))))
	(= 3  (gases/reac:moles-number (first  (gases/reac:<reaction>-products  reac))))
	(= 4  (gases/reac:moles-number (second (gases/reac:<reaction>-products  reac))))
	(= 2  (gases/reac:moles-number (third  (gases/reac:<reaction>-products  reac))))
	(= 1  (gases/reac:moles-number (fourth (gases/reac:<reaction>-products  reac))))))))

(def-test 2*KMnO4+H2O2+3*H2SO4=>3*O2+2*MnSO4+4*H2O+K2SO4-test ()
  " 2*KMnO4+H2O2+3*H2SO4=>3*O2+2*MnSO4+4*H2O+K2SO4  "
  (setf (gases/db:get-sp "KMnO4")
	(make-instance 'gases/db:<sp>
		       :name "KMnO4"
		       :chemical-formula '(("K" 1.0) ("MN" 1.0) ("O" 4.0) ("" 0.0) ("" 0.0))))

  (setf (gases/db:get-sp "MnSO4")
	(make-instance 'gases/db:<sp>
		       :name "MnSO4"
		       :chemical-formula '(("MN" 1.0) ("S" 1.0) ("O" 4.0) ("" 0.0) ("" 0.0))))
  (let ((reac (make-instance 'gases/reac:<reaction>
			     :reactant-names (list "KMnO4" "H2O2" "H2SO4")
			     :product-names  (list  "O2"  "MnSO4" "H2O" "K2SO4" ))))
    (is-true
       (and
	(= 2  (gases/reac:moles-number (first  (gases/reac:<reaction>-reactants reac))))
	(= 1  (gases/reac:moles-number (second (gases/reac:<reaction>-reactants reac))))
	(= 3  (gases/reac:moles-number (third  (gases/reac:<reaction>-reactants reac))))
	(= 3  (gases/reac:moles-number (first  (gases/reac:<reaction>-products  reac))))
	(= 2  (gases/reac:moles-number (second (gases/reac:<reaction>-products  reac))))
	(= 4  (gases/reac:moles-number (third  (gases/reac:<reaction>-products  reac))))
	(= 1  (gases/reac:moles-number (fourth (gases/reac:<reaction>-products  reac))))))))

(def-test 3*FeCl2+2*Na3PO4=>1*Fe3[PO4]2+6*NaCL-test ()
  " 3*FeCl2+2*Na3PO4=>1*Fe3(PO4)2+6*NaCL  "
  (setf (gases/db:get-sp "FeCl2")
	(make-instance 'gases/db:<sp>
		       :name "FeCl2"
		       :chemical-formula '(("Fe" 1.0) ("CL" 2.0) ("" 0.0) ("" 0.0) ("" 0.0))))
  (setf (gases/db:get-sp "Na3PO4")
	(make-instance 'gases/db:<sp>
		       :name "Na3PO4"
		       :chemical-formula '(("Na" 3.0) ("P" 1.0) ("O" 4.0) ("" 0.0) ("" 0.0))))
  (setf (gases/db:get-sp "Fe3(PO4)2")
	(make-instance 'gases/db:<sp>
		       :name "Fe3(PO4)2"
		       :chemical-formula '(("Fe" 3.0) ("P" 2.0) ("O" 8.0) ("" 0.0) ("" 0.0))))
  (setf (gases/db:get-sp "NaCL")
	(make-instance 'gases/db:<sp>
		       :name "NaCL"
		       :chemical-formula '(("Na" 1.0) ("CL" 1.0) ("" 0.0) ("" 0.0) ("" 0.0))))
  (let ((reac   (make-instance 'gases/reac:<reaction>
			       :reactant-names (list "FeCl2"     "Na3PO4" )
			       :product-names  (list "Fe3(PO4)2" "NaCL"))))
    (is-true
     (and
      (= 3  (gases/reac:moles-number (first  (gases/reac:<reaction>-reactants reac))))
      (= 2  (gases/reac:moles-number (second (gases/reac:<reaction>-reactants reac))))
      (= 1  (gases/reac:moles-number (first  (gases/reac:<reaction>-products  reac))))
      (= 6  (gases/reac:moles-number (second (gases/reac:<reaction>-products  reac))))))))


(def-test 3*FeCl2+2*Na3PO4=>1*Fe3[PO4]2+6*NaCL-test ()
  " 4*C8H7N + 39*O2 => 32*CO2 + 14*H2O + 2*N2  "
  (setf (gases/db:get-sp "C8H7N")
	(make-instance 'gases/db:<sp>
		       :name "C8H7N"
		       :chemical-formula '(("C" 8.0) ("H" 7.0) ("N" 1.0) ("" 0.0) ("" 0.0))))
  (let ((reac (make-instance 'gases/reac:<reaction>
			     :reactant-names (list "C8H7N" "O2" )
			     :product-names  (list "CO2" "H2O" "N2"))))
    (is-true
     (and
      (= 1  (gases/reac:moles-number (first  (gases/reac:<reaction>-reactants reac))))
      (= 2  (gases/reac:moles-number (second (gases/reac:<reaction>-reactants reac))))
      (= 1  (gases/reac:moles-number (first  (gases/reac:<reaction>-products  reac))))
      (= 6  (gases/reac:moles-number (second (gases/reac:<reaction>-products  reac))))))))

(def-test test-err-test ()
  (is-true nil))
