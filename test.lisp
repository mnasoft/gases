;;;; test.lisp

(in-package :gases)

(annot:enable-annot-syntax)

(defparameter *equation*
  (list (elements (get-sp "C2H5OH"))
	(elements (get-sp "O2"    ))
	(minus
	 (elements (get-sp "CO2"  )))
	(minus
	 (elements (get-sp "H2O"  )))))

(defun minus (lst)
  (mapcar
   #'(lambda (el)
       (setf (second el) (- (second el)))
       el)
   lst))

(defun make-reaction (reactants products)
  (equation-koeffitients
   (append
	  (mapcar #'(lambda (el) (elements (get-sp el))) reactants)
	  (mapcar #'(lambda (el) (minus (elements (get-sp el)))) products)))
)

(make-reaction '("C2H5OH" "O2") '("CO2" "H2O"))


@export
@annot.doc:doc
"@b(Описание:) функция @b(...)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (atoms *equation*) => (\"C\" \"H\" \"O\")
@end(code)
"
(defun atoms (equation)
  (sort
   (remove-duplicates
    (mapcar #'first (apply #'append equation))
    :test #'string=)
   #'string<))

(defun equation-koeffitients (equation)
  (let ((matr (math:convert-to-triangular 
	       (make-instance
		'math:<matrix>
		:initial-contents 
		(mapcar
		 #'(lambda (el)
		     (nreverse (cons 0 (nreverse el))))
		 (mapcar
		  #'(lambda (el)
		      (mapcar
		       #'(lambda (el1)
			   (if (assoc el el1 :test #'string=)
			       (second (assoc el el1 :test #'string=))
			       0))
		       equation))
		  (atoms equation)))))))
    (when (= 2 (- (math:cols matr) (math:rows matr)))
      (do ((num 2 (1+ num))
	   (denum 1 (1+ denum))
	   (eq (math:row
		(math:solve-linear-system-gauss-backward-run 
		 (make-instance
		  'math:<matrix>
		  :initial-contents
		  (nreverse
		   (cons
		    (loop :for i :downfrom (math:cols matr) :to 1 :collect (if (< i 3) 1 0))
		    (nreverse (math:matrix->2d-list matr))))))
		0)
	       (mapcar #'(lambda (el) (/ (* el num) denum)) eq)))
	  ((every #'integerp eq) eq)))))

(foo *equation*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((sp (get-sp (first '("H2O" "CH4" "N2" "O2" "Air")) ))
      (tt 2500.d0))
  (list (molar-isobaric-heat-capacity sp tt)
	(molar-isochoric-heat-capacity sp tt)
	(molar-enthalpy sp tt)
	(molar-entropy  sp tt)
	(adiabatic-index sp tt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *cmp-1* (make-instance-composition '(("N2" 0.78) ("O2" 0.22))))

(molar-mass *cmp-1*)
(composition-components *cmp-1*)

(molar-enthalpy *cmp-1* (+ *C-0* 25))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format
 t "~{~{~9F ~}~%~}"
 (let ((xx *air*) ;; (get-sp "Air" )
       (rez nil))
   (setf rez 
	 (mapcar
	  #'(lambda (el)
	      (list el
		    (/ (molar-isobaric-heat-capacity  xx (+ *C-0* el)) *kal*)
 		    (/ (molar-isochoric-heat-capacity xx (+ *C-0* el)) *kal*)
;;;;		    
		    (/ (molar-isobaric-heat-capacity  xx (+ *C-0* el))
		       *kal* (molar-mass xx))	      
		    (/ (molar-isochoric-heat-capacity xx (+ *C-0* el))
		       *kal* (molar-mass xx))
;;;;		    
		    (/ (- (molar-enthalpy xx (+ *C-0* el)) (molar-enthalpy xx *C-0*)) 
		       *kal*)
		    (/ (- (molar-enthalpy xx (+ *C-0* el)) (molar-enthalpy xx *C-0*))
		       *kal* (molar-mass xx))
;;;;		    
		    (/ (- (molar-entropy xx (+ *C-0* el)) (molar-entropy xx *C-0*)) 
		       *kal*)
		    (/ (- (molar-entropy xx (+ *C-0* el)) (molar-entropy xx *C-0*))
		       *kal* (molar-mass xx))
		    ))
	  (loop :for i :from 0.0 :to 2500 :by 100.0 :collect i)))
   (push '("  °C" "kal/K*mol" "kal/K*mol"
	   "kkal/K*kg" "kkal/K*kg"
	   " kal/mol"  "kkal/kg"
           "kal/mol*K" "kkal/kg*K")
	 rez)
   (push '("   t" " μcp"      " μcv"
	   "  cp"     "  cv"     "  μi"     "   i"
	   "  μs" "  s"  )
	 rez)
   rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((xx (get-sp "O2" ))
      (el 0.0))
  (/
   (+ (molar-enthalpy xx (+ *C-0* el))
      (- (molar-enthalpy xx 298.15) (molar-enthalpy xx *C-0*))) 
   *kal*))

(get-sp "Air" )



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





