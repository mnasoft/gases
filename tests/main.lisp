(in-package :cl-user)

(defpackage #:gases-tests
  (:use #:cl #:fiveam)
  (:export #:run!
	   #:all-tests
	   #:test-geses))

(in-package #:gases-tests)

(def-suite all-tests
  :description "Мастер-набор всех тестов проекта gases.")

(in-suite all-tests)

(defun test-gases ()
  (run! 'all-tests))


(def-fixture fix-gases ()
  (let ((+Air+ (gases:make-instance-composition
		`(("N2" ,(/ 78.084 100))
		  ("O2" ,(/ 20.9476 100))
		  ("Ar" ,(/ .9365 100))
		  ("CO2",(/ .0319 100)))))

	(+N2+ (gases:make-instance-composition
	       `(("N2" ,(/ 100. 100)))))

	(+O2+ (gases:make-instance-composition
	   `(("O2" ,(/ 100. 100)))))

	(+H2+ (gases:make-instance-composition
	   `(("H2" ,(/ 100. 100)))))

	(+Ar+ (gases:make-instance-composition
	   `(("Ar" ,(/ 100. 100)))))

	(+CO2+ (gases:make-instance-composition
	   `(("CO2" ,(/ 100. 100)))))

	(+running-gas+ (gases:make-instance-composition
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

	(+stopping-gas+ (gases:make-instance-composition
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
    (is-true (math:semi-equal (gases:component-mole-fraction (gethash "N2"  (gases:composition-components +air+))) 0.780840))
    (is-true (math:semi-equal (gases:component-mole-fraction (gethash "O2"  (gases:composition-components +air+))) 0.209476))
    (is-true (math:semi-equal (gases:component-mole-fraction (gethash "CO2" (gases:composition-components +air+))) 0.000319))
    (is-true (math:semi-equal (gases:component-mole-fraction (gethash "Ar"  (gases:composition-components +air+))) 0.009365))
    ))

(def-test molar-fraction-summ-test ()
  (with-fixture fix-gases ()
    (is-true (math:semi-equal (gases:molar-fraction-summ +air+) 1.0))
    (is-true (math:semi-equal (gases:molar-mass +air+)  28.965115))
    (gases:culc-mass-fractions +air+)
    (is-true (math:semi-equal (gases:mass-fraction-summ +air+)  1.0))
    (is-true (gases:check-mole-fraction +air+))
    (is-true (gases:check-mass-fraction +air+))))

(def-test mix-composition-test ()
  (with-fixture fix-gases ()
    (let ((cmp (multiple-value-bind (cmp-1 mfr-1)
		   (gases:mix-composition
		    +N2+ 0.7551837
		    +O2+ 0.23141564)
		 (multiple-value-bind (cmp-2 mfr-2)
		     (gases:mix-composition
		      +Ar+  0.012915987
		      +CO2+ 4.846875e-4)
		   (gases:mix-composition cmp-1 mfr-1 cmp-2 mfr-2)))))
      (is-true (math:semi-equal (gases:component-mole-fraction (gases:reference "N2" +air+))
				(gases:component-mole-fraction (gases:reference "N2" cmp)))))))

(test-gases)


(def-test combustion-reaction-test ()
  (is-true (eq 'gases::<reaction> (type-of (gases:combustion-reaction (gases:get-sp "H2"))))
