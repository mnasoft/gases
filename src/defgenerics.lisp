;;;; defgenerics.lisp

(in-package :gases)



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
  (:documentation"Возвращает сумму мольных долей смеси газов <composition>.
Значение должно равняться единице."))

(defgeneric mass-fraction-summ  (species)
  (:documentation "Возвращает сумму ммассовых долей смеси газов <composition>.
Значение должно равняться единице."))

(defgeneric mix-composition (composition-1 mfr-1 composition-2 mfr-2)
  (:documentation "Возвращает композицию газов как результат смешения 2-х составов с массовыми расходами."))

(defgeneric check-mole-fraction (species)
  (:documentation "Проверка правильности задания мольных долей."))

(defgeneric check-mass-fraction (species)
  (:documentation "Проверка правильности задания массовых долей."))

(defgeneric reference (key container)
  (:documentation "Получает ссылку на элемент, находящийся в конлейнере по ключу."))
(defgeneric elemental-mass-fraction (reference)
  (:documentation "@b(Описание:) метод @b(elemental-mass-fraction) возвращает 
 атомарный состав reference."))

(defgeneric dump (reference stream)
  (:documentation "@b(Описание:) метод @b(dump) сбравывает символьное представление
 reference в символьный поток stream."))

(defgeneric adapt-mole-fractions (reference)
  (:documentation "@b(Описание:) метод @b(adapt-mass-fractions) выполняет подгонку 
состава смеси, заданной ммольными долями."))

(defgeneric  adapt-mass-fractions (reference)
  (:documentation "@b(Описание:) метод @b(adapt-mass-fractions) выполняет подгонку 
состава смеси, заданной массовыми долями."))

(defgeneric insert (obj collection)
  (:documentation "@b(Описание:) обобщенная_функция @b(insert) вставляет объект obj 
в коллекцию collection. "))

(defgeneric combustion-reaction (species)
  (:documentation "@b(Описание:) обобщенная_функция @b(combustion-reaction)"))

(defgeneric relativ-oxigen-mass-for-burning (species)
 (:documentation "@b(Описание:) обобщенная_функция @b(relativ-oxigen-mass-for-burning)
возвращает количество килограмм кислорода (кг), необходимого для сжигания 
одного килограмма топлива."))

(defgeneric relativ-air-mass-for-burning  (species)
 (:documentation "@b(Описание:) обобщенная_функция @b(relativ-air-mass-for-burning)
возвращает количество килограмм воздуха (кг), необходимого для сжигания 
одного килограмма топлива. "))

(defgeneric wobber-hight (species)
  (:documentation "@b(Описание:) обобщенная_функция @b(wobber-hight) возвращает
 число Воббе высшее относительное (по воздуху)."))

(defgeneric wobber-low (species)
  (:documentation "@b(Описание:) обобщенная_функция @b(wobber-low) возвращает
 число Воббе низшее относительное (по воздуху). "))

(defgeneric thermal-effect (species)
  (:documentation "@b(Описание:) обобщенная_функция @b(thermal-effect) возвращает
 тепловой эффект при создании вещества или при химической реакции. "))

(defgeneric Q-work-low (species)
 (:documentation "@b(Описание:) обобщенная_функция @b(Q-work-low) возвращает
 низшую теплотворную способность топлива кДж/кг. "))

(defgeneric density (species pressure temperature)
  (:documentation "@b(Описание:) обобщенная_функция @b(Q-work-low) возвращает
 плотность в кг/м3. "))

(defgeneric density-relative (species pressure temperature &key base-species)
  (:documentation "@b(Описание:) обобщенная_функция @b(Q-work-low) возвращает
 относительную плотность в кг/кг. "))
