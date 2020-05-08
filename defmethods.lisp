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
  (apply #'+ (mapcar #'molar-mass  (composition-components x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-isobaric-heat-capacity                                                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (apply #'+
	 (mapcar
	  #'(lambda (el)
	      (molar-isobaric-heat-capacity el  temperature ))
	  (composition-components x))))

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
  (apply #'+
	 (mapcar #'(lambda (el)
		     (molar-isochoric-heat-capacity el temperature ) )
		 (composition-components x))))

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
  (apply #'+
	 (mapcar
	  #'(lambda (el) (molar-enthalpy el temperature))
	  (composition-components x))))

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

@annot.doc:doc
"Возвращает сумму мольных долей смеси газов <composition>.
Значение должно равняться единице."
(defmethod molar-fraction-summ ((x <composition>))
    (apply #'+
	 (mapcar
	  #'(lambda (el) (component-mole-fraction el))
	  (composition-components x))))


(defmethod mix-composition ((cmp-1 <composition>) (mfr-1 number)
			    (cmp-2 <composition>) (mfr-2 number))
  (mapcar
   #'(lambda (el)
       ;; (sp-molar-mass (component-species el))
       (component-mole-fraction el)
       )
   (composition-components cmp-1)))

(mix-composition *Air* 1.0 *running-gas* 1.0)

(sp-molar-mass (component-species
		(component-mole-fraction
		(first (composition-components *Air*))))

