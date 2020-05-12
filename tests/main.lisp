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


(defparameter *Air*
  (gases:make-instance-composition
   `(("N2" ,(/ 78.0840 100))
     ("O2" ,(/ 20.9476 100))
     ("Ar" ,(/ 00.9365 100))
     ("CO2",(/ 00.0319 100)))))

(gases:component-mole-fraction (gethash "N2" (gases:composition-components *air*))) 0.78084

(def-test component-mole-fraction-test ()
  "Проверка молярного состава."
  (is-true (math:semi-equal (gases:component-mole-fraction (gethash "N2"  (gases:composition-components *air*))) 0.780840))
  (is-true (math:semi-equal (gases:component-mole-fraction (gethash "O2"  (gases:composition-components *air*))) 0.209476))
  (is-true (math:semi-equal (gases:component-mole-fraction (gethash "CO2" (gases:composition-components *air*))) 0.000319))
  (is-true (math:semi-equal (gases:component-mole-fraction (gethash "Ar"  (gases:composition-components *air*))) 0.009365))
  )

(def-test molar-fraction-summ-test ()
  (is-true (math:semi-equal (gases:molar-fraction-summ *air*) 1.0))
  (is-true (math:semi-equal (gases:molar-mass *air*)  28.965115))
  (gases:culc-mass-fractions *air*)
  (is-true (math:semi-equal (gases:mass-fraction-summ *air*)  1.0))
  (is-true (gases:check-mole-fraction *air*))
  (is-true (gases:check-mass-fraction *air*))
  )

(test-gases)
