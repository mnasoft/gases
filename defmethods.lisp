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
;;;; (molar-mass (gethash \"N2\" *sp-db*)) => 28.0134
;;;; (molar-mass (gethash \"CH4\" *sp-db*)) => 16.04246
"
(defmethod molar-mass ((x <sp>))
  (sp-molar-mass x))

@annot.doc:doc
  "Возвращает молекулярную массу, [g/mol]
Пример использования:
;;;; (molar-mass ) => 3.1998801
;;;; (molar-mass (make-instance '<component> :mole-fraction 0.2 :species (gethash \"N2\"  *sp-db*))) => 5.6026797
;;;; (molar-mass (make-instance '<component> :mole-fraction 0.3 :species (gethash \"CH4\" *sp-db*))) => 4.812738
"
(defmethod molar-mass ((x <component>))
  (* (component-mole-fraction  x) (molar-mass (component-species x))))

@annot.doc:doc
  "Возвращает молекулярную массу, [g/mol]
Пример использования:
  (make-instance
   '<composition> :components
   (list (make-instance '<component> :species (gethash \"N2\"              *sp-db*) :mole-fraction 0.0003 )
	 (make-instance '<component> :species (gethash \"CO2\"             *sp-db*) :mole-fraction 0.0022 )
	 (make-instance '<component> :species (gethash \"CH4\"             *sp-db*) :mole-fraction 0.7374 )
	 (make-instance '<component> :species (gethash \"C2H6\"            *sp-db*) :mole-fraction 0.0593 )
	 (make-instance '<component> :species (gethash \"C3H8\"            *sp-db*) :mole-fraction 0.1179 )
	 (make-instance '<component> :species (gethash \"C4H10,isobutane\" *sp-db*) :mole-fraction 0.0131 )
	 (make-instance '<component> :species (gethash \"C4H10,n-butane\"  *sp-db*) :mole-fraction 0.0379 )
	 (make-instance '<component> :species (gethash \"C5H12,i-pentane\" *sp-db*) :mole-fraction 0.0130 )
	 (make-instance '<component> :species (gethash \"C5H12,n-pentane\" *sp-db*) :mole-fraction 0.0139 )
 	 (make-instance '<component> :species (gethash \"C6H14,n-hexane\"  *sp-db*) :mole-fraction 0.0017 )
	 (make-instance '<component> :species (gethash \"C6H10,cyclo-\"    *sp-db*) :mole-fraction 0.0004 )
	 (make-instance '<component> :species (gethash \"C6H10,cyclo-\"    *sp-db*) :mole-fraction 0.0002 )
	 (make-instance '<component> :species (gethash \"C7H16,n-heptane\" *sp-db*) :mole-fraction 0.0001 )
	 (make-instance '<component> :species (gethash \"C6H10,cyclo-\"    *sp-db*) :mole-fraction 0.0001 )
	 (make-instance '<component> :species (gethash \"H2O\"             *sp-db*) :mole-fraction 0.0027 )))
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
				   :species (gethash el *sp-db*))))
	     ((and el-1 (null el-2))
	      (setf (gethash el (composition-components cmp))
		    (make-instance '<component>
				   :mass-fraction
				   (/ (+ (* (component-mass-fraction el-1) mfr-1)
					 0.0)
				      (+ mfr-1 mfr-2))
				   :species (gethash el *sp-db*)))
	      )
	     ((and el-2 (null el-1))
	      (setf (gethash el (composition-components cmp))
		    (make-instance '<component>
				   :mass-fraction
				   (/ (+ (* (component-mass-fraction el-2) mfr-2)
					 0.0)
				      (+ mfr-1 mfr-2))
				   :species (gethash el *sp-db*)))
	      )
	     (t (error "Что-то пошло не так!!!")))))
     (sort (remove-duplicates cmp-keys :test #'string= ) #'string<))
    (values (culc-molar-fractions cmp) (+ mfr-1 mfr-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) функция @b(dump-spices-db)
"
(defun dump-spices-db (&key (func #'(lambda (key value)
				      (format t "~S~%" key)
				      )))
  (maphash func *sp-db*))

;; (first (composition-components *air*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
