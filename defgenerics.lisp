;;;; defgenerics.lisp

(in-package :gases)

(annot:enable-annot-syntax)

@export
@annot.doc:doc
"Возвращает молекулярную массу, [g/mol]"
(defgeneric molar-mass (species))

@export
@annot.doc:doc
"Возвращает мольную изобарную теплоемкость 
- для класса species
- в зависимости от температуры (temperature), [K]."
(defgeneric molar-isobaric-heat-capacity (species temperature))

@export
@annot.doc:doc
"Возвращает мольную изохорую теплоемкость 
- для класса species
- в зависимости от температуры (temperature), [K]"
(defgeneric molar-isochoric-heat-capacity (species temperature))

@export
@annot.doc:doc
"Возвращает мольную энтальпию 
- для класса species
- в зависимости от температуры (temperature), [K]."
(defgeneric molar-enthalpy (species temperature))

@export
@annot.doc:doc
"Возвращает мольную энтальпию 
- для класса species
- в зависимости от температуры (temperature), [K]"
(defgeneric molar-entropy (species temperature))

@export
@annot.doc:doc
"Возвращает показатель адиабаты
- для класса species
- в зависимости от температуры (temperature), [K]"
(defgeneric adiabatic-index (species temperature))

@export
@annot.doc:doc
"Возвращает сумму мольных долей смеси газов <composition>.
Значение должно равняться единице."
(defgeneric molar-fraction-summ (species))

@export
@annot.doc:doc
"Возвращает композицию газов как результат смешения 2-х составов с массовыми расходами."
(defgeneric mix-composition (composition-1 mfr-1 composition-1 mfr-2))

