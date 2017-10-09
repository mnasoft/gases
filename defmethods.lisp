;;;; defmethods.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-mass                                                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-mass ((x molecule))
  "Возвращает молекулярную массу, [g/mol]
Пример использования:
(molar-mass *air*)
(molar-mass *N2*)
(molar-mass *O2*)
"
  (* (molecule-mass x) 1000))

;;;;;;;;;;;;;;;;;;;;

(defmethod molar-mass ((x sp))
  "Возвращает молекулярную массу, [g/mol]
Пример использования
(molar-mass (gethash \"N2\" *sp-db*)) => 28.0134
(molar-mass (gethash \"CH4\" *sp-db*)) => 16.04246
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
  nil
  )

(defmethod molar-isobaric-heat-capacity ((x sp) temperature)
  "Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]"
  nil
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-isochoric-heat-capacity                                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric molar-isochoric-heat-capacity (species temperature)
  (:documentation "Возвращает мольную изохорую теплоемкость 
- для класса species
- в зависимости от температуры (temperature), [K]"))

(defmethod molar-isochoric-heat-capacity ((x sp-rec) temperature)
    "Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
    )

(defmethod molar-isochoric-heat-capacity ((x sp) temperature)
    "Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    adiabatic-index                                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric adiabatic-index (species temperature)
  (:documentation "Возвращает показатель адиабаты
- для класса species
- в зависимости от температуры (temperature), [K]"))

(defmethod adiabatic-index ((x sp-rec) temperature)
    "Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
    )

(defmethod adiabatic-index ((x sp) temperature)
    "Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
    )


(molar-mass (gethash "N2" *sp-db*))
	    
(molar-mass (gethash "CH4" *sp-db*))

(defmethod muCv ((x sp) temperature)
  )


    
(gethash "N2" *sp-db*)

(apply #'+
       (mapcar #'(lambda (el)
		   (let ((elm (gethash (first el) *sp-db*)))
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


