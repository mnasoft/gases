;;;; defmethods.lisp

(in-package :gases)

(annot:enable-annot-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-mass                                                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.doc:doc
"Возвращает молекулярную массу, [g/mol]
Пример использования:
;;;; (molar-mass *air*)
;;;; (molar-mass *N2*)
;;;; (molar-mass *O2*)
"
(defmethod molar-mass ((x <molecule>))
  (* (molecule-mass x) 1000))

;;;;;;;;;;;;;;;;;;;;

@annot.doc:doc
"Возвращает молекулярную массу, [g/mol]
Пример использования
;;;; (molar-mass (get-sp \"N2\" )) => 28.0134
;;;; (molar-mass (get-sp \"CH4\")) => 16.04246
"
(defmethod molar-mass ((x <sp>))
  (sp-molar-mass x))

@annot.doc:doc
  "Возвращает молекулярную массу, [g/mol]
Пример использования:
;;;; (molar-mass (make-instance '<component> :mole-fraction 0.2 :species (get-sp \"N2\" ))) => 5.6026797
;;;; (molar-mass (make-instance '<component> :mole-fraction 0.3 :species (get-sp \"CH4\"))) => 4.812738
"
(defmethod molar-mass ((x <component>))
  (* (component-mole-fraction  x) (molar-mass (component-species x))))

@annot.doc:doc
  "Возвращает молекулярную массу, [g/mol]
Пример использования:
  (make-instance
   '<composition> :components
   (list (make-instance '<component> :species (get-sp \"N2\"              ) :mole-fraction 0.0003 )
	 (make-instance '<component> :species (get-sp \"CO2\"             ) :mole-fraction 0.0022 )
	 (make-instance '<component> :species (get-sp \"CH4\"             ) :mole-fraction 0.7374 )
	 (make-instance '<component> :species (get-sp \"C2H6\"            ) :mole-fraction 0.0593 )
	 (make-instance '<component> :species (get-sp \"C3H8\"            ) :mole-fraction 0.1179 )
	 (make-instance '<component> :species (get-sp \"C4H10,isobutane\" ) :mole-fraction 0.0131 )
	 (make-instance '<component> :species (get-sp \"C4H10,n-butane\"  ) :mole-fraction 0.0379 )
	 (make-instance '<component> :species (get-sp \"C5H12,i-pentane\" ) :mole-fraction 0.0130 )
	 (make-instance '<component> :species (get-sp \"C5H12,n-pentane\" ) :mole-fraction 0.0139 )
 	 (make-instance '<component> :species (get-sp \"C6H14,n-hexane\"  ) :mole-fraction 0.0017 )
	 (make-instance '<component> :species (get-sp \"C6H10,cyclo-\"    ) :mole-fraction 0.0004 )
	 (make-instance '<component> :species (get-sp \"C6H10,cyclo-\"    ) :mole-fraction 0.0002 )
	 (make-instance '<component> :species (get-sp \"C7H16,n-heptane\" ) :mole-fraction 0.0001 )
	 (make-instance '<component> :species (get-sp \"C6H10,cyclo-\"    ) :mole-fraction 0.0001 )
	 (make-instance '<component> :species (get-sp \"H2O\"             ) :mole-fraction 0.0027 )))
"
(defmethod molar-mass ((x <composition>))
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (molar-mass value) rez))
	     (composition-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-isobaric-heat-capacity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.doc:doc
"Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]"
(defmethod molar-isobaric-heat-capacity ((x <sp-rec>) temperature)
  (multiple-value-bind (a1 a2 a3 a4 a5 a6 a7)  (values-list (sp-rec-coefficients x))
    (* *Rμ* (Cp/R-new temperature a1 a2 a3 a4 a5 a6 a7))))

@annot.doc:doc
"Возвращает мольную изобарную теплоемкость muCp, [kJ/(mol*K)]"
(defmethod molar-isobaric-heat-capacity ((x <sp>) temperature)
  (molar-isobaric-heat-capacity
   (find-if
    #'(lambda (el)
	(multiple-value-bind (a b) (values-list (sp-rec-temperature-range el))
	  (<= a temperature b)))
    (sp-reccords x))
   temperature))

@annot.doc:doc
"Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]"
(defmethod molar-isobaric-heat-capacity ((x <component>) temperature)
  (* (component-mole-fraction  x)
     (molar-isobaric-heat-capacity
      (component-species x)
      temperature)))

@annot.doc:doc
"Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]" 
(defmethod molar-isobaric-heat-capacity ((x <composition>) temperature)
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (molar-isobaric-heat-capacity value temperature) rez))
	     (composition-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-isochoric-heat-capacity                                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.doc:doc
"Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
(defmethod molar-isochoric-heat-capacity ((x <sp-rec>) temperature)
  (- (molar-isobaric-heat-capacity x temperature) *Rμ*))

@annot.doc:doc
"Возвращает мольную изохорную теплоемкость muCv, [kJ/(mol*K)]"
(defmethod molar-isochoric-heat-capacity ((x <sp>) temperature)
  (- (molar-isobaric-heat-capacity x temperature) *Rμ*))

@annot.doc:doc
"Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]" 
(defmethod molar-isochoric-heat-capacity ((x <component>) temperature)
  (* (component-mole-fraction x)
     (molar-isochoric-heat-capacity
      (component-species x)
      temperature)))

@annot.doc:doc
"Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
(defmethod molar-isochoric-heat-capacity ((x <composition>) temperature)
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (molar-isochoric-heat-capacity value temperature) rez))
	     (composition-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-enthalpy                                                                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.doc:doc
"Возвращает мольную энтальпию muΗ, [J/(mol*K)]"
(defmethod molar-enthalpy ((x <sp-rec>) temperature)
  (multiple-value-bind (a1 a2 a3 a4 a5 a6 a7 a8)
      (values-list
       (concatenate
	'list
	(sp-rec-coefficients x)
	(list (first (sp-rec-integration-constants x)))))
       (* *Rμ* temperature (H/RT-new temperature a1 a2 a3 a4 a5 a6 a7 a8))))

@annot.doc:doc
"Возвращает мольную энтальпию muΗ, [J/(mol*K)]"
(defmethod molar-enthalpy ((x <sp>) temperature)
  (molar-enthalpy
   (find-if
    #'(lambda (el)
	(multiple-value-bind (a b) (values-list (sp-rec-temperature-range el))
	  (<= a temperature b)))
    (sp-reccords x))
   temperature))

@annot.doc:doc
"Возвращает мольную энтальпию muΗ, [J/(mol*K)]"  
(defmethod molar-enthalpy ((x <component>) temperature)
  (* (component-mole-fraction x)
     (molar-enthalpy
      (component-species x)
      temperature)))

@annot.doc:doc
"Возвращает мольную энтальпию muΗ, [J/(mol*K)]"
(defmethod molar-enthalpy ((x <composition>) temperature)
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (molar-enthalpy value temperature) rez))
	     (composition-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-entropy                                                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.doc:doc
"Возвращает мольную энтропию muS, [J/(mol*K)]"
(defmethod molar-entropy ((x <sp-rec>) temperature)
  (multiple-value-bind (a1 a2 a3 a4 a5 a6 a7 a9)
      (values-list
       (concatenate
	'list
	(sp-rec-coefficients x)
	(list (second (sp-rec-integration-constants x)))))
    (* *Rμ* (S/R-new temperature a1 a2 a3 a4 a5 a6 a7 a9))))

@annot.doc:doc
"Возвращает мольную энтропию muS, [J/(mol*K)]"
(defmethod molar-entropy ((x <sp>) temperature)
  (molar-entropy
   (find-if
    #'(lambda (el)
	(multiple-value-bind (a b) (values-list (sp-rec-temperature-range el))
	  (<= a temperature b)))
    (sp-reccords x))
   temperature))

@annot.doc:doc
"Возвращает мольную энтропию muS, [J/(mol*K)]"
(defmethod molar-entropy ((x <component>) temperature)
  (* (component-mole-fraction x)
     (molar-entropy
      (component-species x)
      temperature)))

@annot.doc:doc
"Возвращает мольную энтропию muS, [J/(mol*K)]"
(defmethod molar-entropy ((x <composition>) temperature)
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (molar-entropy value temperature) rez))
	     (composition-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    adiabatic-index                                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.doc:doc
  "Возвращает показатель адиабаты для класса <sp-rec> 
в зависимости от температуры (temperature), [K]"
(defmethod adiabatic-index ((x <sp-rec>) temperature)
  (/ (molar-isobaric-heat-capacity x temperature)
     (molar-isochoric-heat-capacity x temperature)))

@annot.doc:doc
"Возвращает показатель адиабаты для класса <sp> 
в зависимости от температуры (temperature), [K]"
(defmethod adiabatic-index ((x <sp>) temperature)
  (/ (molar-isobaric-heat-capacity x temperature)
     (molar-isochoric-heat-capacity x temperature)))

@annot.doc:doc
"Возвращает показатель адиабаты для класса <component>
в зависимости от температуры (temperature), [K]"
(defmethod adiabatic-index ((x <component>) temperature)
  (/ (molar-isobaric-heat-capacity x temperature)
     (molar-isochoric-heat-capacity x temperature)))

@annot.doc:doc
"Возвращает показатель адиабаты для класса <composition>
в зависимости от температуры (temperature), [K]"
(defmethod adiabatic-index ((x <composition>) temperature)
  (/ (molar-isobaric-heat-capacity x temperature)
     (molar-isochoric-heat-capacity x temperature)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; molar-fraction-summ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"Возвращает сумму мольных долей смеси газов <composition>.
Значение должно равняться единице."
(defmethod molar-fraction-summ ((x <composition>))
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (component-mole-fraction value) rez))
	     (composition-components x))
    (apply #'+ rez)))

@export
@annot.doc:doc
"Возвращает сумму мольных долей смеси газов <composition>.
Значение должно равняться единице."
(defmethod mass-fraction-summ ((x <composition>))
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (component-mass-fraction value) rez))
	     (composition-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.doc:doc
"@b(Описание:) метод @b(mix-composition) возвращает список, состоящий из
компонентного состава, выраженного в мольных долях, и массового расхода.
"
(defmethod mix-composition ((cmp-1 <composition>) (mfr-1 number)
			    (cmp-2 <composition>) (mfr-2 number))
  (let ((cmp-keys nil)
	(cmp (make-instance '<composition>)))
    (maphash #'(lambda (key value) (push key cmp-keys))(composition-components cmp-1))
    (maphash #'(lambda (key value) (push key cmp-keys))(composition-components cmp-2))
    (mapcar
     #'(lambda (el)
	 (let ((el-1 (gethash el (composition-components cmp-1)))
	       (el-2 (gethash el (composition-components cmp-2))))
	   (cond
	     ((and el-1 el-2)
	      (setf (gethash el (composition-components cmp))
		    (make-instance '<component>
				   :mass-fraction
				   (/ (+ (* (component-mass-fraction el-1) mfr-1)
					 (* (component-mass-fraction el-2) mfr-2))
				      (+ mfr-1 mfr-2))
				   :species (get-sp el))))
	     ((and el-1 (null el-2))
	      (setf (gethash el (composition-components cmp))
		    (make-instance '<component>
				   :mass-fraction
				   (/ (+ (* (component-mass-fraction el-1) mfr-1)
					 0.0)
				      (+ mfr-1 mfr-2))
				   :species (get-sp el)))
	      )
	     ((and el-2 (null el-1))
	      (setf (gethash el (composition-components cmp))
		    (make-instance '<component>
				   :mass-fraction
				   (/ (+ (* (component-mass-fraction el-2) mfr-2)
					 0.0)
				      (+ mfr-1 mfr-2))
				   :species (get-sp el)))
	      )
	     (t (error "Что-то пошло не так!!!")))))
     (sort (remove-duplicates cmp-keys :test #'string= ) #'string<))
    (values (culc-molar-fractions cmp) (+ mfr-1 mfr-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) метод @b(culc-mass-fractions) вычисляет массовые доли компонентов
композиции газов на основании мольного состава.
"
(defmethod culc-mass-fractions ((cmp <composition>))
  (let ((mm (molar-mass cmp)))
    (maphash 
     #'(lambda (key value)
	 (declare (ignore key))
	 (setf (component-mass-fraction value)
	       (/ (molar-mass value) mm)))
     (composition-components cmp))
    cmp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.doc:doc
"@b(Описание:) метод @b(mass-molar) вспомогательный.
Используется в @b(culc-molar-fractions).
"
(defmethod mass-molar ((x <composition>))
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (/ (component-mass-fraction value)
			  (sp-molar-mass (component-species value)))
		       rez))
	     (composition-components x))
    (apply #'+ rez)))

@export
@annot.doc:doc
"@b(Описание:) метод @b(culc-molar-fractions) вычисляет массовые доли компонентов
композиции газов на основании мольного состава.
"
(defmethod culc-molar-fractions ((cmp <composition>))
  (let ((mm (mass-molar cmp)))
    (maphash 
     #'(lambda (key value)
	 (declare (ignore key))
	 (setf (component-mole-fraction value)
	       (/ (component-mass-fraction value)
		  (sp-molar-mass (component-species value))
		  mm)))
     (composition-components cmp))
    cmp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"Проверка правильности задания мольных долей."
(defmethod check-mole-fraction ((cmp <composition>))
  (math:semi-equal (molar-fraction-summ cmp) 1.0))

@export
@annot.doc:doc
"Проверка правильности задания массовых долей."
(defmethod check-mass-fraction ((cmp <composition>))
  (math:semi-equal (mass-fraction-summ cmp) 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"Получает ссылку на элемент, находящийся в конлейнере по ключу."
(defmethod reference ((key string) (cmp <composition>))
  (gethash key (composition-components cmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) метод @b(elemental-mass-fraction) 
вычисляет массовые доли элементарного состава (поатомного) композиции.
Возвращает композицию, состоящую из атомов, соответствующих элементов и
сумму массовых долей компонентов (для проверки).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (elemental-mass-fraction *running-gas*)
 => #<composition>(
    #<component>(name=\"C\" mole-fraction=0.23387624 mass-fraction=0.7807496)
    #<component>(name=\"H\" mole-fraction=0.76500297 mass-fraction=0.21431628)
    #<component>(name=\"N\" mole-fraction=8.96593e-5 mass-fraction=3.4905068e-4)
    #<component>(name=\"O\" mole-fraction=0.0010310818 mass-fraction=0.0045851567))
    0.99999994
@end(code)
"
(defmethod elemental-mass-fraction ((cmp <composition>))
  (let ((rez nil))
    (maphash
     #'(lambda (key value)
	 (declare (ignore key))
	 (push (elemental-mass-fraction value) rez))  
     (composition-components cmp))
    (values-list
     (reduce
      #'(lambda (x y)
	  (multiple-value-list 
	   (mix-composition (first x) (second x) (first y) (second y))))
      rez
      :initial-value (list (make-instance '<composition>) 0.0)))))

@export
@annot.doc:doc
"@b(Описание:) метод @b(elemental-mass-fraction) 
вычисляет массовые доли элементарного состава (поатомного) смеси.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (elemental-mass-fraction (reference \"CO2\" *running-gas*))
@end(code)
"
(defmethod elemental-mass-fraction ((ref <component>))
  (let ((cmp (make-instance '<composition>)))
    (map nil
	 #'(lambda (el)
	     (setf (component-mass-fraction el)
		   (/
		    (* (sp-molar-mass (component-species el))
		       (component-mass-fraction el))
		    (sp-molar-mass (component-species ref))))
	     (setf (gethash
		    (sp-name (component-species el))
		    (composition-components cmp))
		   el))
	 (mapcar
	  #'(lambda (el)
	      (setf (first el)
		    (string-capitalize (first el)))
	      (make-instance '<component>
			     :species (get-sp (first el))
			     :mass-fraction (second el)))
	  (remove-if
	   #'(lambda (el)
	       (or (and (numberp (second el)) (= 0.0 (second el)))
		   (and (stringp (first el)) (string= "" (first el)))))
	   (sp-chemical-formula (component-species ref)))))
    (list cmp (component-mass-fraction ref))))

@export
@annot.doc:doc
"@b(Описание:) метод @b(elemental-mass-fraction) 

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gases:elemental-mass-fraction (get-sp \"H2O2(L)\"))
@end(code)
"
(defmethod elemental-mass-fraction ((ref <sp>))
  (let ((cmp (make-instance '<composition>)))
    (map nil
	 #'(lambda (el)
	     (setf (component-mass-fraction el)
		   (/
		    (* (sp-molar-mass (component-species el))
		       (component-mass-fraction el))
		    (sp-molar-mass ref)))
	     (setf (gethash
		    (sp-name (component-species el))
		    (composition-components cmp))
		   el))
	 (mapcar
	  #'(lambda (el)
	      (setf (first el)
		    (string-capitalize (first el)))
	      (make-instance '<component>
			     :species (get-sp (first el))
			     :mass-fraction (second el)))
	  (remove-if
	   #'(lambda (el)
	       (or (and (numberp (second el)) (= 0.0 (second el)))
		   (and (stringp (first el)) (string= "" (first el)))))
	   (sp-chemical-formula ref))))
    (list cmp 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) метод @b(elements) возвращает атомарный состав компонента.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (elements (get-sp \"C2H5OH\")) 
 => ((\"C\" 2) (\"H\" 6) (\"O\" 1))
@end(code)
"
(defmethod elements ((ref <sp>))
  (mapcar
   #'(lambda (el-1)
       (list (first el-1) (round (second el-1))))
   (remove-if
   #'(lambda (el)
       (or (and (numberp (second el)) (= 0.0 (second el)))
	   (and (stringp (first el)) (string= "" (first el)))))
   (sp-chemical-formula ref))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defmethod molar-mass ((rt <reactant>))
  (* (moles-number rt) (sp-molar-mass (reactant-species rt))))

@export
(defmethod molar-mass ((pt <product>))
  (* (moles-number pt) (sp-molar-mass (product-species pt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defmethod thermal-effect ((rt <reactant>))
  (* -1
     (moles-number rt)
     (sp-heat-formation (reactant-species rt))))

@export
(defmethod thermal-effect ((rt <product>))
  (* (moles-number rt)
     (sp-heat-formation (product-species rt))))

@export
(defmethod thermal-effect ((reac <reaction>))
  (apply #'+
   (mapcar #'thermal-effect
   (append (reaction-reactants reac) (reaction-products reac)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun append-some-value-to-length (new-len value lst)
  (loop :for i :from 0 :below new-len
	:collect
	(let ((l (nth i lst)))
	  (if  l l value))))

@annot.doc:doc
"@b(Описание:) метод @b(dump) выполняет вывод объекта sp в поток s.
 Вывод должен осуществляться в форме пригодной для последующего считывания 
 в формате TermoBuild.
"
(defmethod dump ((sp <sp>) s)
  (labels ((rec () (first (sp-reccords sp))))
    (format s "~16A  ~62A~%" (sp-name sp) (sp-comments sp))
    (format s "~2D ~6A ~{~2A~6,2F~}~2D~13,7f~15,3f~%"
	    (sp-number-temperature-intervals sp)
	    (sp-reference-date-code sp) 
	    (apply #'append (sp-chemical-formula sp))
	    (sp-phase sp) 
	    (sp-molar-mass sp) 
	    (sp-heat-formation sp))
    (when (/= 0 (sp-number-temperature-intervals sp))
      (map nil #'(lambda (el)(dump el s))
	   (sp-reccords sp)))
    (when (= 0 (sp-number-temperature-intervals sp))
      (format s "~{~11,3f~}~1D~{~5,1F~}  ~15,3F~%" ;
	      (sp-rec-temperature-range (rec))
	      (sp-rec-number-coeff (rec))
	      (append-some-value-to-length
	       8 0.0 (sp-rec-polynomial-exponents (rec))) ;; Проверить считыватель
	      (sp-rec-h_298.15-h-0 (rec))))))

(defmethod dump+ ((sp <sp>) s)
  (labels ((rec () (first (sp-reccords sp))))
    (format s "~16A  ~62A~%" (sp-name sp) (sp-comments sp))
    (format s "~2D ~6A ~{~2A~6,2F~}~2D~13,7f~15,3f~%"
	    (sp-number-temperature-intervals sp)
	    (sp-reference-date-code sp) 
	    (apply #'append (sp-chemical-formula sp))
	    (sp-phase sp) 
	    (sp-molar-mass sp) 
	    (sp-heat-formation sp))
    (when (/= 0 (sp-number-temperature-intervals sp))
      (map nil #'(lambda (el) (dump+ el s))
	   (sp-reccords sp)))
    (when (= 0 (sp-number-temperature-intervals sp))
      (format s "~{~11,3f~}~1D~{~5,1F~}  ~15,3F~%" ;
	      (sp-rec-temperature-range (rec))
	      (sp-rec-number-coeff (rec))
	      (append-some-value-to-length
	       8 0.0 (sp-rec-polynomial-exponents (rec))) ;; Проверить считыватель
	      (sp-rec-h_298.15-h-0 (rec))))))

(defmethod dump+d->e ((sp <sp>) s)
  (labels ((rec () (first (sp-reccords sp))))
    (format s "~16A  ~62A~%" (sp-name sp) (sp-comments sp))
    (format s "~2D ~6A ~{~2A~6,2F~}~2D~13,7f~15,3f~%"
	    (sp-number-temperature-intervals sp)
	    (sp-reference-date-code sp) 
	    (apply #'append (sp-chemical-formula sp))
	    (sp-phase sp) 
	    (sp-molar-mass sp) 
	    (sp-heat-formation sp))
    (when (/= 0 (sp-number-temperature-intervals sp))
      (map nil #'(lambda (el)(dump+d->e el s))
	   (sp-reccords sp)))
    (when (= 0 (sp-number-temperature-intervals sp))
      (format s "~{~11,3f~}~1D~{~5,1F~}  ~15,3F~%" ;
	      (sp-rec-temperature-range (rec))
	      (sp-rec-number-coeff (rec))
	      (append-some-value-to-length
	       8 0.0 (sp-rec-polynomial-exponents (rec))) ;; Проверить считыватель
	      (sp-rec-h_298.15-h-0 (rec))))))

(defun lst-from-below (from below replace-nil-with lst)
  "Пример использования:
 (lst-from-below 0 5 0d0 '( 1 2 3 4 5 6 7 ))
 (lst-from-below 5 8 (make-string 16 :initial-element #\Space) '( 1 2 3 4 5 6 7 ))
"
    (substitute
     replace-nil-with nil 
     (loop :for i :from from :below below
	   :collect
	   (nth i lst))))

(defmethod dump ((rec <sp-rec>) s)
  (labels ((fmt-16-9 (lst)
	     (mapcar
	      #'(lambda (el)
		  (if (and (numberp el)(= el 0.0d0)) " 0.000000000D+00" el))
	      lst)))
    (format s "~{~11,3f~}~1D~{~5,1F~}  ~15,3F~%"
	    (sp-rec-temperature-range rec)
	    (sp-rec-number-coeff rec)
	    (append-some-value-to-length
	     8 0.0 (sp-rec-polynomial-exponents rec)) ;; Проверить считыватель
	    (sp-rec-h_298.15-h-0 rec))
    (format s "~{~16,9,2E~}~%"
	    (fmt-16-9
	     (lst-from-below 0 5
			     (make-string 16 :initial-element #\Space)
			     (sp-rec-coefficients rec))))
    (format s "~{~16,9,2E~}~{~16,9,2E~}~%"
	    (fmt-16-9
	     (lst-from-below 5 8
			     (make-string 16 :initial-element #\Space)
			     (sp-rec-coefficients rec)))
	    (fmt-16-9 (sp-rec-integration-constants rec)))))

(defmethod dump+ ((rec <sp-rec>) s)
  (labels ((fmt-16-9 (lst)
	     (mapcar
	      #'(lambda (el)
		  (if (and (numberp el)(= el 0.0d0)) " 0.000000000D+00" el))
	      lst)))
    (format s " ~{~10,3f~} ~1D~{~5,1F~}  ~15,3F~%"
	    (sp-rec-temperature-range rec)
	    (sp-rec-number-coeff rec)
	    (append-some-value-to-length
	     8 0.0 (sp-rec-polynomial-exponents rec)) ;; Проверить считыватель
	    (sp-rec-h_298.15-h-0 rec))
    (format s "~{~16,9,2E~}~%"
	    (fmt-16-9
	     (lst-from-below 0 5 0.0d0
			     (sp-rec-coefficients rec))))
    (format s "~{~16,9,2E~}~{~16,9,2E~}~%"
	    (fmt-16-9
	     (lst-from-below 5 8 0.0d0
			     (sp-rec-coefficients rec)))
	    (fmt-16-9 (sp-rec-integration-constants rec)))))

(defmethod dump+d->e ((rec <sp-rec>) s)
  (labels ((fmt-16-9 (lst)
	     (mapcar
	      #'(lambda (el)
		  (if (and (numberp el)(= el 0.0d0)) " 0.000000000E+00" el))
	      lst)))
    (format s " ~{~10,3f~} ~1D~{~5,1F~}  ~15,3F~%"
	    (sp-rec-temperature-range rec)
	    (sp-rec-number-coeff rec)
	    (append-some-value-to-length
	     8 0.0 (sp-rec-polynomial-exponents rec)) ;; Проверить считыватель
	    (sp-rec-h_298.15-h-0 rec))
    (format s "~{~16,9,2,,,,'EE~}~%"
	    (fmt-16-9
	     (lst-from-below 0 5 0.0d0
			     (sp-rec-coefficients rec))))
    (format s "~{~16,9,2,,,,'EE~}~{~16,9,2,,,,'EE~}~%"
	    (fmt-16-9
	     (lst-from-below 5 8 0.0d0
			     (sp-rec-coefficients rec)))
	    (fmt-16-9 (sp-rec-integration-constants rec)))))

@export
@annot.doc:doc
"Сброс БД, загруженной в хештаблицу, в поток s."
(defmethod dump ((ht hash-table) s)
  (maphash
   #'(lambda (key value)
       (declare (ignore key))
       (dump value s))
   ht))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun substringp (needle haystack &key (test #'char=))
  "Returns the index of the first occurrence of the string designated
by NEEDLE within the string designated by HAYSTACK, or NIL if it does
not occur.  Characters within the string are compared by TEST, which
defaults to CHAR= (for case-sensitive comparison)."
  (search (string needle)
          (string haystack)
          :test test))


(defun get-db-as-string ()
  (when (null *str-db*)
    (setf *str-db*
	  (file-get-contents
	   (namestring (asdf:system-relative-pathname :gases "data/termo.inp")))))
  *str-db*)


(defmethod check-sp ((sp <sp>))
  (let* ((o-str (make-string-output-stream ))
	 (sp-str (progn
		   (dump sp o-str)
		   (get-output-stream-string o-str)))
	 (sp-str+ (progn
		    (dump+ sp o-str)
		    (get-output-stream-string o-str)))
	 (sp-str+d->e (progn
			(dump+d->e sp o-str)
			(get-output-stream-string o-str))))
    (if (or (substringp sp-str      (get-db-as-string))
	    (substringp sp-str+     (get-db-as-string))
	    (substringp sp-str+d->e (get-db-as-string)))
	t
	(progn (format t "~S~%~%~S~%~%~S~%"
		       sp-str sp-str+ sp-str+d->e)
	       nil))))

(defmethod check-sp ((sp-name string))
  (check-sp (get-sp sp-name)))

(defun check-db ()
  (let ((rezult t)
	(bad-keys nil))
    (maphash
     #'(lambda (key value)
	 (unless (check-sp value)
	   (setf rezult nil)
	   (push key bad-keys))
	 (format t "."))
     (get-db))
    (when bad-keys (format t "~&~S~%" bad-keys))
    (values rezult bad-keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun property-table (xx)
  (let ((rez nil))
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

@export
(defun print-table (xx &key (stream t) (output-fortmat :text))
  "Пример использования:
 (print-table *air* :output-fortmat :text)
"
  (assert (member output-fortmat '(:text :org)))
  (let ((tbl (property-table xx)))
    (format stream "~A~%" xx)
    (when (eq output-fortmat :text)
      (format stream "~{~{~9F ~}~%~}" tbl))
    (when (eq output-fortmat :org)
      (format stream "~{~{| ~9F ~}|~%~}" tbl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert ((c <component>) (cmp <composition>))
  (setf (gethash (sp-name (component-species c)) (composition-components cmp)) c)
  cmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; adapt

@export
@annot.doc:doc
"@b(Описание:) метод @b(adapt-mass-fractions) выполняет подгонку 
состава смеси, заданной ммольными долями.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn
  (defparameter *cmp* (make-instance '<composition>))
  (insert (make-instance-component  \"N2\" 0.5) *cmp*)
  (insert (make-instance-component  \"O2\" 0.4) *cmp*))
@end(code)
"
(defmethod adapt-mole-fractions ((cmp <composition>))
  (culc-molar-fractions (culc-mass-fractions cmp)))

@export
@annot.doc:doc
"@b(Описание:) метод @b(adapt-mass-fractions) выполняет подгонку 
состава смеси, заданной массовыми долями.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn
  (defparameter *cmp* (make-instance '<composition>))
  (insert (make-instance-component  \"N2\" 0.51 :mass) *cmp*)
  (insert (make-instance '<component> :species (get-sp \"O2\") :mass-fraction 0.4) *cmp*)
  (insert (make-instance '<component> :species (get-sp \"H2\") :mass-fraction 0.1) *cmp*)
  (adapt-mass-fractions *cmp*))
@end(code)
"
(defmethod  adapt-mass-fractions ((cmp <composition>))
  (culc-mass-fractions (culc-molar-fractions cmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; combustion-reaction

@export
@annot.doc:doc
"@b(Описание:) функция|метод|обобщенная_функция| @b(...)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (combustion-reaction (get-sp \"CO\")) => 2*CO + 1*O2 => 2*CO2
@end(code)
"
(defmethod combustion-reaction ((sp <sp>))
  (let ((cmp (first (elemental-mass-fraction sp)))
	(good-combasted nil)
	(combasted-elem '("H" "C" "S"))
	(good-fuel      t)
	(fuel-elem '("H" "C" "S" "O" "N"))
	(reactants nil)
	(products  nil))
    (block not-combastor-sp
      (when (gethash (sp-name sp) *not-combasted-sp*)
	(return-from not-combastor-sp nil)))
    (block check-and-make-reaction
      (maphash
       #'(lambda (key value)
	   (declare (ignore value))
	   (block good-combasted-elements
	     (if (member key combasted-elem :test #'string=) 
		 (setf good-combasted (or good-combasted t))
		 (setf good-combasted (or good-combasted nil))))
	   (block good-combasted-elems
	     (if (member key fuel-elem  :test #'string=)  
		 (setf good-fuel (and good-fuel t))
		 (setf good-fuel (and good-fuel nil)))))
       (composition-components cmp))
      (block stop-error-block
	(unless good-combasted
	  (error "Bad combasted elements present in fuel~%Топливо ~S~%В топливе присутствуют горючие элементы кроме ~S"
		 sp combasted-elem))
	(unless good-fuel
	  (error "Bad elements present in fuel~%Топливо ~S~%В топливе элементы кроме ~S"
		 sp fuel-elem)))
      (block compose-reaction
	(block compose-reactants
	  (push "O2" reactants)
	  (push (sp-name sp) reactants))
	(block compose-products
	  (when (reference "N" cmp) (push "N2" products ))
	  (when (reference "S" cmp) (push "SO2" products ))
	  (when (reference "C" cmp) (push "CO2" products ))
	  (when (reference "H" cmp) (push "H2O" products )))
	(make-instance '<reaction>
		       :reactant-names reactants
		       :product-names products)))))

@export
(defmethod combustion-reaction ((cmp <component>))
  (combustion-reaction (component-species cmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; relativ-oxigen-mass-for-burning

(defmethod relativ-oxigen-mass-for-burning ((sp <sp>))
  (let ((reactants (reaction-reactants (combustion-reaction sp))))
    (/ (molar-mass (second reactants)) (molar-mass (first reactants)))))

(defmethod relativ-oxigen-mass-for-burning ((cmp <component>))
  (let ((reactants (reaction-reactants (combustion-reaction cmp))))
    (* (component-mass-fraction cmp)
       (/ (molar-mass (second reactants)) (molar-mass (first reactants))))))

(defmethod relativ-oxigen-mass-for-burning ((cmp <composition>))
  (let ((components (composition-components cmp))
	(rez nil))
    (maphash
     #'(lambda (key value)
	 (declare (ignore key))
	 (push (relativ-oxigen-mass-for-burning value) rez))
     components)
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; relativ-air-mass-for-burning

(defmethod relativ-air-mass-for-burning ((sp <sp>))
  (/ (relativ-oxigen-mass-for-burning sp)
     (component-mass-fraction (reference "O2" *air*))))

(defmethod relativ-air-mass-for-burning ((cmp <component>))
  (/ (relativ-oxigen-mass-for-burning cmp)
     (component-mass-fraction (reference "O2" *air*))))

(defmethod relativ-air-mass-for-burning ((cmp <composition>))
  (/ (relativ-oxigen-mass-for-burning cmp)
     (component-mass-fraction (reference "O2" *air*))))
