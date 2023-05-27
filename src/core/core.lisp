;;;; package.lisp

(defpackage gases/core
  (:use cl gases/const gases/db)
  (:export molar-mass
           molar-isobaric-heat-capacity 
           molar-isochoric-heat-capacity 
           molar-enthalpy 
           molar-entropy 
           adiabatic-index 
           molar-fraction-summ 
           mass-fraction-summ  
           mix-composition 
           check-mole-fraction 
           check-mass-fraction 
           reference 
           elemental-mass-fraction 
           adapt-mole-fractions 
           adapt-mass-fractions 
           insert 
           density 
           density-relative)
  (:export cp/r-old
           h/rt-old
           s/r-old)
  (:export cp/r-new
           h/rt-new
           s/r-new)
  (:export make-instance-component
           make-instance-composition)
  (:export <component>
           species
           mole-fraction
           mass-fraction)
  (:export <composition>
           <composition>-components
           culc-mass-fractions
           culc-molar-fractions
           elements)
  (:export find-atoms
           q-of
           find-by-atoms
           print-table))
#+nil
(declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

;;;; gases.lisp

(in-package gases/core)

(defun Cp/R-old (TT a1 a2 a3 a4 a5) ; a6 a7
  "Возвращает мольную теплоемкость отнесенную к универсальной газовой постоянной.
см.
https://www.grc.nasa.gov/www/CEAWeb/RP-1311.pdf p.20
The NASA polynomials have the form:
Cp/R = a1 + a2 T + a3 T^2 + a4 T^3 + a5 T^4    (4.6)
"
  (+ a1
     (* a2 TT)
     (* a3 TT TT)
     (* a4 TT TT TT)
     (* a5 TT TT TT TT)))

(defun H/RT-old (TT a1 a2 a3 a4 a5 a6) ; a7
  "Возвращает мольную энтальпию отнесенную к универсальной газовой постоянной и абсолютной температуре.
см.
https://www.grc.nasa.gov/www/CEAWeb/RP-1311.pdf p.20
The NASA old polynomials have the form:
H/RT = a1 + a2 T /2 + a3 T^2 /3 + a4 T^3 /4 + a5 T^4 /5 + a6/T    (4.7)
"
  (+ a1
     (* a2 TT 1/2)
     (* a3 TT TT 1/3)
     (* a4 TT TT TT 1/4)
     (* a5 TT TT TT TT 1/5)
     (/ a6 TT)))

(defun S/R-old (TT a1 a2 a3 a4 a5 a7) ; a6
  "Возвращает мольную энтропию отнесенную к универсальной газовой постоянной.
см.
https://www.grc.nasa.gov/www/CEAWeb/RP-1311.pdf p.20
The NASA old polynomials have the form:
S/R  = a1 lnT + a2 T + a3 T^2 /2 + a4 T^3 /3 + a5 T^4 /4 + a7    (4.8)
"
  (+ (* a1 (log TT))
     (* a2 TT)
     (* a3 TT TT 1/2)
     (* a4 TT TT TT 1/3)
     (* a5 TT TT TT TT 1/4)
     a7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Cp/R-new (TT a1 a2 a3 a4 a5 a6 a7) ; a8 a9
  "Возвращает мольную теплоемкость отнесенную к универсальной газовой постоянной.
см.
https://www.grc.nasa.gov/www/CEAWeb/RP-1311.pdf p.20
The NASA new polynomials have the form:
Cp/R = a1 T^-2 + a2 T^-1 + a3 + a4 T + a5 T^2 + a6 T^3 + a7 T^4    (4.9)
"
  (+ (/ a1 TT TT)
     (/ a2 TT)
     a3
     (* a4 TT)
     (* a5 TT TT)
     (* a6 TT TT TT)
     (* a7 TT TT TT TT)))

(defun H/RT-new (TT a1 a2 a3 a4 a5 a6 a7 a8 ) ; a9
  "Возвращает мольную энтальпию отнесенную к универсальной газовой постоянной и абсолютной температуре.
см.
https://www.grc.nasa.gov/www/CEAWeb/RP-1311.pdf p.20
The NASA new polynomials have the form:
H/RT = -a1 T^-2 + a2 T^-1 ln T + a3 T^2 /3 + a4 T /2 + a5 T^2 /3 + a6 T^3 /4 + a7 T^4 /5 + a8/T    (4.10)
"  
  (+ (/ a1 TT TT -1)
     (* a2 (/ TT) (log TT))
     a3
     (* a4 TT 1/2)
     (* a5 TT TT 1/3)
     (* a6 TT TT TT 1/4)
     (* a7 TT TT TT TT 1/5)
     (/ a8 TT)))

(defun S/R-new (TT a1 a2 a3 a4 a5 a6 a7 a9) ; a8 
  "Возвращает мольную энтропию отнесенную к универсальной газовой постоянной.
см.
https://www.grc.nasa.gov/www/CEAWeb/RP-1311.pdf p.20
The NASA new polynomials have the form:
S/R  = -a1 T^-2 /2 - a2 T^-1 + a3 lnT + a4 T + a5 T^2 /2 + a6 T^3 /3 + a7 T^4 /4 + a9    (4.11)
"
  (+ (/ a1 TT TT -2)
     (/ a2 TT -1)
     (* a3 (log TT))
     (* a4 TT)
     (* a5 TT TT 1/2)
     (* a6 TT TT TT 1/3)
     (* a7 TT TT TT TT 1/4)
     a9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; classes.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <component> ()
  ((species :accessor species :initarg :species
	    :documentation
	    "Должен содержать объект типа <sp>.")
   (mole-fraction :accessor mole-fraction :initarg
		  :mole-fraction :initform 0.0 :documentation
		  "Содежит мольную долю компонета в смеси.")
   (mass-fraction :accessor mass-fraction :initarg :mass-fraction :initform 0.0
		  :documentation
		  "Содежит массовую долю компонета в смеси."))
  (:documentation
   "Представляет компонент смеси, заданной мольными долями."))

(defmethod print-object :before ((x <component>) s)
  (format s
	  "#<component>(name=~S mole-fraction=~S mass-fraction=~S"
	  (<sp>-name (species x)) (mole-fraction x) (mass-fraction x)))

(defmethod print-object         ((x <component>) s) (format s "" ))

(defmethod print-object :after  ((x <component>) s) (format s ")" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <composition> nil
  ((components :accessor <composition>-components :initarg :components
	       :documentation
	       "Содержит список компонентов. 
Элементами этого списка д.б. данные типа <component>"))

  (:documentation
   "Представляет смесь, состоящую из объектов класса <component>."))

(defun check-spices-exist-in-db (lst)
"Проверка на то, что заданные компоненты имеются в базе данных."
  (reduce
   #'(lambda (el1 el2)
       (and el1 (get-sp (first el2))))
   lst
   :initial-value t))

(defun check-spices-is-unique (lst)
"Проверка на неповторяемость компопнентов, передаваемых в конструктор."
  (= (length lst)
     (length
      (remove-duplicates
       (mapcar #'first lst)
       :test #'string=))))

(defmethod initialize-instance :after ((cmp <composition>)
				       &key (components (make-hash-table :test #'equal)))
  (setf (<composition>-components cmp) components))

(defmethod print-object :before ((x <composition>) s)
  (format s
	  "#<composition>(")
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (format s "~%~S" value))
	   (<composition>-components x)))

(defmethod print-object ((x <composition>) s)
  (format s "" ))

(defmethod print-object :after ((x <composition>) s)
  (format s ")" ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; termo.lisp

;;;; make-instance 

(defun make-instance-component (component-name fraction &optional (fraction-type :mole))
"@b(Описание:) функция @b(make-instance-component) возвращает компонент
газовой смеси, заданной мольными или массовыми долями. По умолчанию
поределяется через мольную долю.

@b(Переменые:)
@begin(list)
@item(component-name - имя компонента;)
@item(fraction - доля компонента мольная или массовая;)
@item(fraction-type - тип доли. :mole задает мольную долю; :mass - массовую.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-instance-component \"N2\" 0.78)
 (make-instance-component \"O2\" 0.22)
 (make-instance-component \"N2\" 0.78 :mass)
 (make-instance-component \"O2\" 0.78 :mass)
@end(code)
"
  (ecase fraction-type
  (:mole (make-instance '<component>
		 :species (get-sp component-name)
		 :mole-fraction fraction))
  (:mass (make-instance '<component>
		 :species (get-sp component-name)
		 :mass-fraction fraction))))

(defun make-instance-composition (lst &optional (fraction-type :mole))
"@b(Описание:) функция @b(make-instance-composition) возвращает 
газовую смесь, заданную мольными долями.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-instance-composition '((\"N2\" 0.78) (\"O2\" 0.22)))
 (make-instance-composition '((\"N2\" 0.78) (\"O2\" 0.22)) :mole)
 (make-instance-composition '((\"N2\" 0.78) (\"O2\" 0.22)) :mass)
@end(code)
"
  (unless (check-spices-is-unique lst)
    (error "Spices is not unique=~S" (check-spices-is-unique lst) ))
  (unless (check-spices-exist-in-db lst)
    (error "Some spices is not exist in db=~S" (check-spices-exist-in-db lst) ))
  (let ((cpm-s (make-hash-table :test #'equal)))
    (mapcar #'(lambda(el)
		(setf
		 (gethash (first el) cpm-s)
		 (make-instance-component (first el)(second el) fraction-type)))
	    lst)
    (let ((cmp (make-instance '<composition> :components cpm-s)))
      (ecase fraction-type
	(:mole
	 (unless (check-mole-fraction cmp)
	   (error "Mole fraction summ=~S not equal 1.0"
		  (molar-fraction-summ cmp)))
	 (culc-mass-fractions cmp))
	(:mass
	 (unless (check-mass-fraction cmp)
	   (error "Mass fraction summ=~S not equal 1.0"
		  (mass-fraction-summ cmp)))
	 (culc-molar-fractions cmp)))
      cmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defgenerics.lisp

(defgeneric molar-mass (species)
  (:documentation "Возвращает молекулярную массу, [g/mol]"))

(defgeneric molar-isobaric-heat-capacity (species temperature)
  (:documentation "Возвращает мольную изобарную теплоемкость 
- для класса species
- в зависимости от температуры (temperature), [K]."))

(defgeneric molar-isochoric-heat-capacity (species temperature)
  
  (:documentation "Возвращает мольную изохорую теплоемкость 
- для класса species
- в зависимости от температуры (temperature), [K]"))

(defgeneric molar-enthalpy (species temperature)
  (:documentation "Возвращает мольную энтальпию 
- для класса species
- в зависимости от температуры (temperature), [K]."))

(defgeneric molar-entropy (species temperature)
  (:documentation "Возвращает мольную энтальпию 
- для класса species
- в зависимости от температуры (temperature), [K]"))

(defgeneric adiabatic-index (species temperature)
  (:documentation "Возвращает показатель адиабаты
- для класса species
- в зависимости от температуры (temperature), [K]"
		  ))

(defgeneric molar-fraction-summ (species)
  (:documentation "Возвращает сумму мольных долей смеси газов
<composition>.  Значение должно равняться единице."))

(defgeneric mass-fraction-summ  (species)
  (:documentation
   "Возвращает сумму ммассовых долей смеси газов <composition>.
  Значение должно равняться единице."))

(defgeneric mix-composition (composition-1 mfr-1 composition-2 mfr-2)
  (:documentation "Возвращает композицию газов как результат смешения
  2-х составов с массовыми расходами."))

(defgeneric check-mole-fraction (species)
  (:documentation "Проверка правильности задания мольных долей."))

(defgeneric check-mass-fraction (species)
  (:documentation "Проверка правильности задания массовых долей."))

(defgeneric reference (key container)
  (:documentation "Получает ссылку на элемент, находящийся в
  конлейнере по ключу."))

(defgeneric elemental-mass-fraction (reference)
  (:documentation "@b(Описание:) метод @b(elemental-mass-fraction)
 возвращает атомарный состав reference."))

(defgeneric adapt-mole-fractions (reference)
  (:documentation "@b(Описание:) метод @b(adapt-mass-fractions)
выполняет подгонку состава смеси, заданной ммольными долями."))

(defgeneric  adapt-mass-fractions (reference)
  (:documentation "@b(Описание:) метод @b(adapt-mass-fractions)
выполняет подгонку состава смеси, заданной массовыми долями."))

(defgeneric insert (obj collection)
  (:documentation "@b(Описание:) обобщенная_функция @b(insert)
вставляет объект obj в коллекцию collection. "))

(defgeneric density (species pressure temperature)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(Q-work-low) возвращает
 плотность в кг/м3. "))

(defgeneric density-relative (species pressure temperature &key base-species)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(Q-work-low) возвращает
 относительную плотность в кг/кг. "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defmethods.lisp
;;;; molar-mass 

(defmethod molar-mass ((x <sp>))
"Возвращает молекулярную массу, [g/mol]
Пример использования
;;;; (molar-mass (get-sp \"N2\" )) => 28.0134
;;;; (molar-mass (get-sp \"CH4\")) => 16.04246
"
  (<sp>-molar-mass x))

(defmethod molar-mass ((x <component>))
  "Возвращает молекулярную массу, [g/mol]
Пример использования:
;;;; (molar-mass (make-instance '<component> :mole-fraction 0.2 :species (get-sp \"N2\" ))) => 5.6026797
;;;; (molar-mass (make-instance '<component> :mole-fraction 0.3 :species (get-sp \"CH4\"))) => 4.812738
"
  (* (mole-fraction  x) (molar-mass (species x))))

(defmethod molar-mass ((x <composition>))
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
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (molar-mass value) rez))
	     (<composition>-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-isobaric-heat-capacity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-isobaric-heat-capacity ((x <sp-rec>) temperature)
"Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]"
  (multiple-value-bind (a1 a2 a3 a4 a5 a6 a7)  (values-list (<sp-rec>-coefficients x))
    (* +Rμ+ (Cp/R-new temperature a1 a2 a3 a4 a5 a6 a7))))

(defmethod molar-isobaric-heat-capacity ((x <sp>) temperature)
"Возвращает мольную изобарную теплоемкость muCp, [kJ/(mol*K)]"
  (molar-isobaric-heat-capacity
   (find-if
    #'(lambda (el)
	(multiple-value-bind (a b) (values-list (<sp-rec>-temperature-range el))
	  (<= a temperature b)))
    (<sp>-reccords x))
   temperature))

(defmethod molar-isobaric-heat-capacity ((x <component>) temperature)
"Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]"
  (* (mole-fraction  x)
     (molar-isobaric-heat-capacity
      (species x)
      temperature)))

(defmethod molar-isobaric-heat-capacity ((x <composition>) temperature)
"Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]" 
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (molar-isobaric-heat-capacity value temperature) rez))
	     (<composition>-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-isochoric-heat-capacity                                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-isochoric-heat-capacity ((x <sp-rec>) temperature)
"Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
  (- (molar-isobaric-heat-capacity x temperature) +Rμ+))

(defmethod molar-isochoric-heat-capacity ((x <sp>) temperature)
"Возвращает мольную изохорную теплоемкость muCv, [kJ/(mol*K)]"
  (- (molar-isobaric-heat-capacity x temperature) +Rμ+))

(defmethod molar-isochoric-heat-capacity ((x <component>) temperature)
"Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]" 
  (* (mole-fraction x)
     (molar-isochoric-heat-capacity
      (species x)
      temperature)))

(defmethod molar-isochoric-heat-capacity ((x <composition>) temperature)
"Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (molar-isochoric-heat-capacity value temperature) rez))
	     (<composition>-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-enthalpy                                                                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-enthalpy ((x <sp-rec>) temperature)
"Возвращает мольную энтальпию muΗ, [J/(mol*K)]"
  (multiple-value-bind (a1 a2 a3 a4 a5 a6 a7 a8)
      (values-list
       (concatenate
	'list
	(<sp-rec>-coefficients x)
	(list (first (<sp-rec>-integration-constants x)))))
    (* +Rμ+ temperature (H/RT-new temperature a1 a2 a3 a4 a5 a6 a7 a8))))

(defmethod molar-enthalpy ((x <sp>) temperature)
"Возвращает мольную энтальпию muΗ, [J/(mol*K)]"
  (molar-enthalpy
   (find-if
    #'(lambda (el)
	(multiple-value-bind (a b) (values-list (<sp-rec>-temperature-range el))
	  (<= a temperature b)))
    (<sp>-reccords x))
   temperature))

(defmethod molar-enthalpy ((x <component>) temperature)
"Возвращает мольную энтальпию muΗ, [J/(mol*K)]"  
  (* (mole-fraction x)
     (molar-enthalpy
      (species x)
      temperature)))

(defmethod molar-enthalpy ((x <composition>) temperature)
"Возвращает мольную энтальпию muΗ, [J/(mol*K)]"
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (molar-enthalpy value temperature) rez))
	     (<composition>-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-entropy                                                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-entropy ((x <sp-rec>) temperature)
"Возвращает мольную энтропию muS, [J/(mol*K)]"
  (multiple-value-bind (a1 a2 a3 a4 a5 a6 a7 a9)
      (values-list
       (concatenate
	'list
	(<sp-rec>-coefficients x)
	(list (second (<sp-rec>-integration-constants x)))))
    (* +Rμ+ (S/R-new temperature a1 a2 a3 a4 a5 a6 a7 a9))))

(defmethod molar-entropy ((x <sp>) temperature)
"Возвращает мольную энтропию muS, [J/(mol*K)]"
  (molar-entropy
   (find-if
    #'(lambda (el)
	(multiple-value-bind (a b) (values-list (<sp-rec>-temperature-range el))
	  (<= a temperature b)))
    (<sp>-reccords x))
   temperature))

(defmethod molar-entropy ((x <component>) temperature)
"Возвращает мольную энтропию muS, [J/(mol*K)]"
  (* (mole-fraction x)
     (molar-entropy
      (species x)
      temperature)))

(defmethod molar-entropy ((x <composition>) temperature)
"Возвращает мольную энтропию muS, [J/(mol*K)]"
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (molar-entropy value temperature) rez))
	     (<composition>-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    adiabatic-index                                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod adiabatic-index ((x <sp-rec>) temperature)
  "Возвращает показатель адиабаты для класса <sp-rec> 
в зависимости от температуры (temperature), [K]"
  (/ (molar-isobaric-heat-capacity x temperature)
     (molar-isochoric-heat-capacity x temperature)))

(defmethod adiabatic-index ((x <sp>) temperature)
"Возвращает показатель адиабаты для класса <sp> 
в зависимости от температуры (temperature), [K]"
  (/ (molar-isobaric-heat-capacity x temperature)
     (molar-isochoric-heat-capacity x temperature)))

(defmethod adiabatic-index ((x <component>) temperature)
"Возвращает показатель адиабаты для класса <component>
в зависимости от температуры (temperature), [K]"
  (/ (molar-isobaric-heat-capacity x temperature)
     (molar-isochoric-heat-capacity x temperature)))

(defmethod adiabatic-index ((x <composition>) temperature)
"Возвращает показатель адиабаты для класса <composition>
в зависимости от температуры (temperature), [K]"
  (/ (molar-isobaric-heat-capacity x temperature)
     (molar-isochoric-heat-capacity x temperature)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; molar-fraction-summ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-fraction-summ ((x <composition>))
"Возвращает сумму мольных долей смеси газов <composition>.
Значение должно равняться единице."
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (mole-fraction value) rez))
	     (<composition>-components x))
    (apply #'+ rez)))

(defmethod mass-fraction-summ ((x <composition>))
"Возвращает сумму мольных долей смеси газов <composition>.
Значение должно равняться единице."
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (mass-fraction value) rez))
	     (<composition>-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mix-composition ((cmp-1 <composition>) (mfr-1 number)
			    (cmp-2 <composition>) (mfr-2 number))
"@b(Описание:) метод @b(mix-composition) возвращает список, состоящий из
компонентного состава, выраженного в мольных долях, и массового расхода.
"
  (let ((cmp-keys nil)
	(cmp (make-instance '<composition>)))
    (maphash #'(lambda (key value) (push key cmp-keys))(<composition>-components cmp-1))
    (maphash #'(lambda (key value) (push key cmp-keys))(<composition>-components cmp-2))
    (mapcar
     #'(lambda (el)
	 (let ((el-1 (gethash el (<composition>-components cmp-1)))
	       (el-2 (gethash el (<composition>-components cmp-2))))
	   (cond
	     ((and el-1 el-2)
	      (setf (gethash el (<composition>-components cmp))
		    (make-instance '<component>
				   :mass-fraction
				   (/ (+ (* (mass-fraction el-1) mfr-1)
					 (* (mass-fraction el-2) mfr-2))
				      (+ mfr-1 mfr-2))
				   :species (get-sp el))))
	     ((and el-1 (null el-2))
	      (setf (gethash el (<composition>-components cmp))
		    (make-instance '<component>
				   :mass-fraction
				   (/ (+ (* (mass-fraction el-1) mfr-1)
					 0.0)
				      (+ mfr-1 mfr-2))
				   :species (get-sp el)))
	      )
	     ((and el-2 (null el-1))
	      (setf (gethash el (<composition>-components cmp))
		    (make-instance '<component>
				   :mass-fraction
				   (/ (+ (* (mass-fraction el-2) mfr-2)
					 0.0)
				      (+ mfr-1 mfr-2))
				   :species (get-sp el)))
	      )
	     (t (error "Что-то пошло не так!!!")))))
     (sort (remove-duplicates cmp-keys :test #'string= ) #'string<))
    (values (culc-molar-fractions cmp) (+ mfr-1 mfr-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod culc-mass-fractions ((cmp <composition>))
"@b(Описание:) метод @b(culc-mass-fractions) вычисляет массовые доли компонентов
композиции газов на основании мольного состава.
"
  (let ((mm (molar-mass cmp)))
    (maphash 
     #'(lambda (key value)
	 (declare (ignore key))
	 (setf (mass-fraction value)
	       (/ (molar-mass value) mm)))
     (<composition>-components cmp))
    cmp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mass-molar ((x <composition>))
"@b(Описание:) метод @b(mass-molar) вспомогательный.
Используется в @b(culc-molar-fractions).
"
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (/ (mass-fraction value)
			  (<sp>-molar-mass (species value)))
		       rez))
	     (<composition>-components x))
    (apply #'+ rez)))

(defmethod culc-molar-fractions ((cmp <composition>))
"@b(Описание:) метод @b(culc-molar-fractions) вычисляет массовые доли компонентов
композиции газов на основании мольного состава.
"
  (let ((mm (mass-molar cmp)))
    (maphash 
     #'(lambda (key value)
	 (declare (ignore key))
	 (setf (mole-fraction value)
	       (/ (mass-fraction value)
		  (<sp>-molar-mass (species value))
		  mm)))
     (<composition>-components cmp))
    cmp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod check-mole-fraction ((cmp <composition>))
"Проверка правильности задания мольных долей."
  (math/core:semi-equal (molar-fraction-summ cmp) 1.0))

(defmethod check-mass-fraction ((cmp <composition>))
"Проверка правильности задания массовых долей."
  (math/core:semi-equal (mass-fraction-summ cmp) 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reference ((key string) (cmp <composition>))
"Получает ссылку на элемент, находящийся в конлейнере по ключу."
  (gethash key (<composition>-components cmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod elemental-mass-fraction ((cmp <composition>))
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
  (let ((rez nil))
    (maphash
     #'(lambda (key value)
	 (declare (ignore key))
	 (push (elemental-mass-fraction value) rez))  
     (<composition>-components cmp))
    (values-list
     (reduce
      #'(lambda (x y)
	  (multiple-value-list 
	   (mix-composition (first x) (second x) (first y) (second y))))
      rez
      :initial-value (list (make-instance '<composition>) 0.0)))))

(defmethod elemental-mass-fraction ((ref <component>))
"@b(Описание:) метод @b(elemental-mass-fraction) 
вычисляет массовые доли элементарного состава (поатомного) смеси.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (elemental-mass-fraction (reference \"CO2\" *running-gas*))
@end(code)
"
  (let ((cmp (make-instance '<composition>)))
    (map nil
	 #'(lambda (el)
	     (setf (mass-fraction el)
		   (/
		    (* (<sp>-molar-mass (species el))
		       (mass-fraction el))
		    (<sp>-molar-mass (species ref))))
	     (setf (gethash
		    (<sp>-name (species el))
		    (<composition>-components cmp))
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
	   (<sp>-chemical-formula (species ref)))))
    (list cmp (mass-fraction ref))))

(defmethod elemental-mass-fraction ((ref <sp>))
"@b(Описание:) метод @b(elemental-mass-fraction) 

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gases:elemental-mass-fraction (get-sp \"H2O2(L)\"))
@end(code)
"
  (let ((cmp (make-instance '<composition>)))
    (map nil
	 #'(lambda (el)
	     (setf (mass-fraction el)
		   (/
		    (* (<sp>-molar-mass (species el))
		       (mass-fraction el))
		    (<sp>-molar-mass ref)))
	     (setf (gethash
		    (<sp>-name (species el))
		    (<composition>-components cmp))
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
	   (<sp>-chemical-formula ref))))
    (list cmp 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod elements ((ref <sp>))
"@b(Описание:) метод @b(elements) возвращает атомарный состав компонента.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (elements (get-sp \"C2H5OH\")) 
 => ((\"C\" 2) (\"H\" 6) (\"O\" 1))
@end(code)
"
  (mapcar
   #'(lambda (el-1)
       (list (first el-1) (round (second el-1))))
   (remove-if
   #'(lambda (el)
       (or (and (numberp (second el)) (= 0.0 (second el)))
	   (and (stringp (first el)) (string= "" (first el)))))
   (<sp>-chemical-formula ref))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun property-table (xx)
  (let ((rez nil))
     (setf rez 
	   (mapcar
	    #'(lambda (el)
		(list el
		      (/ (molar-isobaric-heat-capacity  xx (+ gases/const:+c-0+ el)) gases/const:+kal+)
 		      (/ (molar-isochoric-heat-capacity xx (+ gases/const:+c-0+ el)) gases/const:+kal+)
;;;;		    
		      (/ (molar-isobaric-heat-capacity  xx (+ gases/const:+c-0+ el))
			 gases/const:+kal+ (molar-mass xx))	      
		      (/ (molar-isochoric-heat-capacity xx (+ gases/const:+c-0+ el))
			 gases/const:+kal+ (molar-mass xx))
;;;;		    
		      (/ (- (molar-enthalpy xx (+ gases/const:+c-0+ el)) (molar-enthalpy xx gases/const:+c-0+)) 
			 gases/const:+kal+)
		      (/ (- (molar-enthalpy xx (+ gases/const:+c-0+ el)) (molar-enthalpy xx gases/const:+c-0+))
			 gases/const:+kal+ (molar-mass xx))
;;;;		    
		      (/ (- (molar-entropy xx (+ gases/const:+c-0+ el)) (molar-entropy xx gases/const:+c-0+)) 
			 gases/const:+kal+)
		      (/ (- (molar-entropy xx (+ gases/const:+c-0+ el)) (molar-entropy xx gases/const:+c-0+))
			 gases/const:+kal+ (molar-mass xx))
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
  (setf (gethash (<sp>-name (species c)) (<composition>-components cmp)) c)
  cmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; adapt

(defmethod adapt-mole-fractions ((cmp <composition>))
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
  (culc-molar-fractions (culc-mass-fractions cmp)))

(defmethod  adapt-mass-fractions ((cmp <composition>))
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
  (culc-mass-fractions (culc-molar-fractions cmp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod density ((sp <sp>) pressure temperature)
  (/ (* (molar-mass sp) pressure)
     (* +Rμ+ temperature)
     1000))

(defmethod density ((c-t <component>) pressure temperature)
  (/ (* (molar-mass c-t) pressure)
     (* +Rμ+ temperature)
     1000))

(defmethod density ((c-n <composition>) pressure temperature)
  (/ (* (molar-mass c-n) pressure)
     (* +Rμ+ temperature)
     1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; select.lisp

(defmacro q-of (elem func quan)
  `(find-if
    #'(lambda (el)
	(and (string= (first el) ,elem)
	     (,func (second el) ,quan)))
    formula))

(defmacro find-atoms (elem rule)
"@b(Описание:) макрос @b(find-atoms)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-atoms (get-sp \"H2O\") (and (q-of \"H\"  =  2) (q-of \"O\" =  1) t))
 (find-atoms (get-sp \"H2O\") (and (q-of \"NA\" =  1) (q-of \"CL\" = 1) t))
@end(code)
"
  `(let ((formula (<sp>-chemical-formula ,elem)))
     ,rule))

(defmacro find-by-atoms (rule)
"@b(Описание:) макрос @b(find-by-atoms) позволяет выполнять поиск 
веществ в базе данных по количеству атомов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-by-atoms (and (q-of \"C\" >= 2) (q-of \"H\" = 6) (q-of \"O\" = 1)))
 => (\"C2H5OH\" \"CH3OCH3\" \"C3H6O,propylox\" \"C3H6O,acetone\" \"C3H6O,propanal\"
 \"C6H5OH,phenol\")
@end(code)
 (find-by-atoms (and (q-of \"H\" >= 2) (q-of \"H\" <= 3) (q-of \"O\" = 1))) 
 => (\"HBOH\" \"HCHO,formaldehy\" \"H2BOH\" \"H2O\" \"NH2OH\" \"CH2OH\" \"CH3O\" \"CH2CO,ketene\"
     \"CH3CO,acetyl\" \"H2O(cr)\" \"H2O(L)\")
"
  `(let ((rez nil))
     (maphash
      #'(lambda (key value)
	  (when
	      (find-atoms value ,rule)
	  (push key rez))) 
     (get-db))
     rez))
