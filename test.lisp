;;;; test.lisp

(in-package :gases)

(annot:enable-annot-syntax)



@annot.doc:doc
"Создает замыкание, позволяющее получать индексы эмементов куба (гиппокуба, гиперкуба)
 по слоям в направлении роста индексов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((f (make-layer-iterator 1)))
 (loop :for i :from 0 :to 10 :collect 
   (funcall f))) => '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11))
 (let ((f (make-layer-iterator 2)))
 (loop :for i :from 0 :to 10 :collect 
   (funcall f)))
  =>  ((1 1)                    ; Первая плоскость
       (1 2) (2 1)              ; Вторая плоскость
       (1 3) (2 2) (3 1)        ; Третья плоскость
       (1 4) (2 3) (3 2) (4 1) 
       (1 5) ...)

 (let ((f (make-layer-iterator 3)))
 (loop :for i :from 0 :to 10 :collect 
   (funcall f)))
  => ((1 1 1)                                         ; Первая плоскость
      (1 1 2) (1 2 1) (2 1 1)                         ; Вторая плоскость
      (1 1 3) (1 2 2) (1 3 1) (2 1 2) (2 2 1) (3 1 1) ; Третья плоскость
      (1 1 4) ...)

@end(code)
"
(defun make-layer-iterator (vars)
  (labels ((summ-values (v layer)
	     (+ (length v ) layer -1))
	   (grow-vector (vec layer)
	     (let ((summ-values (summ-values vec layer))
		   (step-summ 0)
		   (v-rez (make-array `(,(length vec)) :initial-element nil )))
	       (loop :for v :across vec
		     :for i :from 0 :below (length vec) :do
		       (setf step-summ (+ step-summ v)
			     (svref v-rez i)
			     (- summ-values
				step-summ
				(- (length vec) 1 i))))
	       v-rez)))
    (let ((lll 1)
	  (vvv (make-array `(,vars) :initial-element 1)))
      #'(lambda () 
	  (let ((rez (coerce vvv 'list))
		(pos (position-if #'(lambda (el) (/= 0 el))
				  (grow-vector vvv lll) :from-end t)))
	    (if pos
		(if (= 0 (- (length vvv) 2 pos))
		    (progn
		      (decf (svref vvv (1- (length vvv))))
		      (incf (svref vvv pos)))
		    (progn
		      (incf (svref vvv pos))
		      (loop :for i :from (1+ pos) :below (length vvv) :do
			(setf (svref vvv i) 1))
		      (setf (svref vvv (1- (length vvv)))
			    (- (summ-values vvv lll)
			       (apply #'+ (cdr (nreverse (coerce vvv 'list))))))))
		(progn
		  (incf lll)
		  (loop :for i :from 0 :below (length vvv) :do
		    (setf (svref vvv i) 1))
		  (setf (svref vvv (1- (length vvv)))
			(- (summ-values vvv lll)
			   (apply #'+ (cdr (nreverse (coerce vvv 'list))))))))
	    rez)))))

(defparameter *f* (make-layer-iterator 2))
(loop :for i :from 0 :to 100 :do
  (format t "~S~%" (funcall *f*)))

(summ-values *v* *l*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (remove-method #'ADAPT-MOLE-FRACTIONS (find-method #'ADAPT-MOLE-FRACTIONS '() (mapcar #'find-class '(t t))))

(with-open-file (fl "/home/namatv/quicklisp/local-projects/clisp/gases/data/termo.inp"
		    :direction :output :if-exists :supersede)
  (dump (get-db) fl))



(relativ-air-mass-for-burning (make-instance-composition '(("CH4" 0.1) ("C6H6" 0.9)) :mass))


(defparameter *sp* (get-sp "CH4"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-sp "Air")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *rt*  
  (make-instance '<reactant> :species (get-sp "H2O") :mole 2))

(molar-mass *rt*)

(thermal-effect *rt*)

(sp-heat-formation (reactant-species *rt*))



(defparameter *reac*
  (make-instance '<reaction> :reactant-names '("C2H5OH" "O2") :product-names '("H2O" "CO2")))

(defparameter *reac*
  (make-instance '<reaction> :reactant-names '("C2H5OH" "O2") :product-names '("H2O(L)" "CO2")))

(defparameter *reac*
  (make-instance '<reaction> :reactant-names '("H2" "O2") :product-names '("H2O")))

(defparameter *reac*
  (make-instance '<reaction> :reactant-names '("CH4" "O2") :product-names '("H2O(L)" "CO2")))

(thermal-effect *reac*)

(culc-koeffitients *reac*)

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
		   (let ((elm (get-sp (first el) )))
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





