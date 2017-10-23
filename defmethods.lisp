;;;; defmethods.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-mass                                                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-mass ((x molecule))
  "Возвращает молекулярную массу, [g/mol]
Пример использования:
;;;; (molar-mass *air*)
;;;; (molar-mass *N2*)
;;;; (molar-mass *O2*)
"
  (* (molecule-mass x) 1000))

;;;;;;;;;;;;;;;;;;;;

(defmethod molar-mass ((x sp))
  "Возвращает молекулярную массу, [g/mol]
Пример использования
;;;; (molar-mass (gethash \"N2\" *sp-db*)) => 28.0134
;;;; (molar-mass (gethash \"CH4\" *sp-db*)) => 16.04246
"
  (sp-molar-mass x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-isobaric-heat-capacity                                                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric molar-isobaric-heat-capacity (species temperature)
  (:documentation "Возвращает мольную изобарную теплоемкость 
- для класса species
- в зависимости от температуры (temperature), [K]"))

(defmethod molar-isobaric-heat-capacity ((x sp-rec) temperature)
  "Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]"
  (multiple-value-bind (a1 a2 a3 a4 a5 a6 a7)  (values-list (sp-rec-coefficients x))
    (* *Rμ* (Cp/R-new temperature a1 a2 a3 a4 a5 a6 a7))))

(defmethod molar-isobaric-heat-capacity ((x sp) temperature)
  "Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]"
  (molar-isobaric-heat-capacity
   (find-if
    #'(lambda (el)
	(multiple-value-bind (a b) (values-list (sp-rec-temperature-range el))
	  (<= a temperature b)))
    (sp-reccords x))
   temperature))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-isochoric-heat-capacity                                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric molar-isochoric-heat-capacity (species temperature)
  (:documentation "Возвращает мольную изохорую теплоемкость 
- для класса species
- в зависимости от температуры (temperature), [K]"))

(defmethod molar-isochoric-heat-capacity ((x sp-rec) temperature)
  "Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
  (- (molar-isobaric-heat-capacity x temperature) *Rμ*))

(defmethod molar-isochoric-heat-capacity ((x sp) temperature)
  "Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
  (- (molar-isobaric-heat-capacity x temperature) *Rμ*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-enthalpy                                                                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric molar-enthalpy (species temperature)
  (:documentation "Возвращает мольную энтальпию 
- для класса species
- в зависимости от температуры (temperature), [K]"))

(defmethod molar-enthalpy ((x sp-rec) temperature)
  "Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
  (multiple-value-bind (a1 a2 a3 a4 a5 a6 a7 a8)
      (values-list
       (concatenate
	'list
	(sp-rec-coefficients x)
	(list (first (sp-rec-integration-constants x)))))
       (* *Rμ* temperature (H/RT-new temperature a1 a2 a3 a4 a5 a6 a7 a8))))

(defmethod molar-enthalpy ((x sp) temperature)
  "Возвращает мольную энтальпию muΗ, [J/(mol*K)]"
  (molar-enthalpy
   (find-if
    #'(lambda (el)
	(multiple-value-bind (a b) (values-list (sp-rec-temperature-range el))
	  (<= a temperature b)))
    (sp-reccords x))
   temperature))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-entropy                                                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric molar-entropy (species temperature)
  (:documentation "Возвращает мольную энтальпию 
- для класса species
- в зависимости от температуры (temperature), [K]"))

(defmethod molar-entropy ((x sp-rec) temperature)
  "Возвращает мольную энтропию muS, [?J/(mol*K)]"
  (multiple-value-bind (a1 a2 a3 a4 a5 a6 a7 a9)
      (values-list
       (concatenate
	'list
	(sp-rec-coefficients x)
	(list (second (sp-rec-integration-constants x)))))
    (* *Rμ* (S/R-new temperature a1 a2 a3 a4 a5 a6 a7 a9))))

(defmethod molar-entropy ((x sp) temperature)
  "Возвращает мольную энтропию muS, [?J/(mol*K)]"
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

(defgeneric adiabatic-index (species temperature)
  (:documentation "Возвращает показатель адиабаты
- для класса species
- в зависимости от температуры (temperature), [K]"))

(defmethod adiabatic-index ((x sp-rec) temperature)
  "Возвращает показатель адиабаты
- для класса sp-rec
- в зависимости от температуры (temperature), [K]"
  (/ (molar-isobaric-heat-capacity x temperature)
     (molar-isochoric-heat-capacity x temperature)))

(defmethod adiabatic-index ((x sp) temperature)
  "Возвращает показатель адиабаты
- для класса sp-rec
- в зависимости от температуры (temperature), [K]"
  (/ (molar-isobaric-heat-capacity x temperature)
     (molar-isochoric-heat-capacity x temperature)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



