;;;; package.lisp

(defpackage gases
  (:use cl )
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
           ;;dump 
           adapt-mole-fractions 
           adapt-mass-fractions 
           insert 
           combustion-reaction 
           relativ-oxigen-mass-for-burning 
           relativ-air-mass-for-burning  
           wobber-hight 
           wobber-low 
           thermal-effect 
           Q-work-low 
           density 
           density-relative)
  (:export 
           get-sp
           cp/r-new
           mole-fraction

           )

  (:export make-instance-component
           make-instance-composition
           )
  (:export 

   <product>
   <component>
   <composition>
   <reactant>
   <reaction>

   
   species
   composition-components

   culc-mass-fractions
   h/rt-old
   h/rt-new
   mass-fraction
   elements

   clear-db

   init-db

   s/r-new
             
   reaction-products
             
   moles-number

   get-db

   culc-molar-fractions
             
   cp/r-old reaction-reactants

   find-atoms q-of find-by-atoms
   print-table

   )) 


;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gases.lisp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'Cp/R-old )
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

(export 'H/RT-old )
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

(export 'Cp/R-new )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'H/RT-new )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'S/R-new )
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
;;;; defparameters.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; classes.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '<component>)
(export 'species)
(export 'mole-fraction)
(export 'mass-fraction)

(defclass <component> ()
  ((component-species :accessor species :initarg :species
		      :documentation
		      "Должен содержать объект типа <sp>.")
   (component-mole-fraction :accessor mole-fraction :initarg
			    :mole-fraction :initform 0.0 :documentation
			    "Содежит мольную долю компонета в смеси.")
   (component-mass-fraction :accessor mass-fraction :initarg :mass-fraction :initform 0.0
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

(export '<composition>)
(export 'composition-components )

(defclass <composition> nil
  ((composition-components :accessor composition-components :initarg :components
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
  (setf (composition-components cmp) components))

(defmethod print-object :before ((x <composition>) s)
  (format s
	  "#<composition>(")
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (format s "~%~S" value))
	   (composition-components x)))

(defmethod print-object ((x <composition>) s)
  (format s "" ))

(defmethod print-object :after ((x <composition>) s)
  (format s ")" ))

(export '<reactant>)
(export 'species)
(export 'moles-number )

(defclass <reactant> ()
  ((reactant-species :accessor species :initarg :species
		      :documentation
		      "Должен содержать объект типа <sp>.")
   (moles-number :accessor moles-number :initarg :mole :initform nil
		 :documentation "Количество молей реактанта, участвующих в химической реакции."))
  (:documentation
   "Представляет реактант химической реакции."))

(defmethod print-object ((rt <reactant>) s)
  (format s "~A*~A" (moles-number rt) (<sp>-name (species rt))))

(defmethod elements ((rt <reactant>))
  (elements (species rt)))

(export '<product>)
(export 'species)
(export 'moles-number)

(defclass <product> ()
  ((product-species :accessor species :initarg :species
		      :documentation
		      "Должен содержать объект типа <sp>.")
   (moles-number :accessor moles-number :initarg :mole :initform nil
		 :documentation "Количество молей продукта, получаемого а результате химической реакции."))
  (:documentation
   "Представляет продукт химической реакции."))

(defmethod print-object ((pt <product>) s)
  (format s "~A*~A" (moles-number pt) (<sp>-name (species pt))))

(defmethod elements ((pt <product>))
  (labels ((minus (lst)
	     (mapcar
	      #'(lambda (el) (list (first el) (- (second el))))
	      lst)))
    (minus (elements (species pt)))))

(export '<reaction>)
(export 'reaction-reactants)
(export 'reaction-products )

(defclass <reaction> ()
  ((reaction-reactants :accessor reaction-reactants :initform nil
		       :documentation "Список реактантов химической реакции.")
   (reaction-products  :accessor reaction-products :initform nil
   		       :documentation "Список реактантов химической реакции."))
  (:documentation
   "Представляет продукт химической реакции."))

(defmethod initialize-instance :after ((react <reaction>)
				       &key (reactant-names nil)
					 (product-names nil))
  (when reactant-names
    (setf (reaction-reactants react)
	  (mapcar #'(lambda (el) (make-instance '<reactant> :species (get-sp el)))
	   reactant-names)))
  (when product-names
    (setf (reaction-products react)
	  (mapcar #'(lambda (el) (make-instance '<product> :species (get-sp el)))
		  product-names)))
  (culc-koeffitients react)
  react)

(defmethod print-object ((reac <reaction>) s)
  (format s "~{~A~^ + ~} => ~{~A~^ + ~}"
	  (reaction-reactants reac) (reaction-products  reac)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-layer-iterator (vars)
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

(defun atoms (equation)
  "@b(Описание:) функция @b(atoms) возвращает список элементов,
 из которых состоит молекула.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (atoms *equation*) => (\"C\" \"H\" \"O\")
@end(code)
"
  (sort
   (remove-duplicates
    (mapcar #'first (apply #'append equation))
    :test #'string=)
   #'string<))

(defun make-linear-index-set ( &key (max-index 1000))
  (loop :for i :from 1 :to max-index :collect `(,i)))

(defun diagonal-index ( x )
  (loop :for i :from 1 :below x
	:collect (list i (- x i))))

(defun make-diagonal-index-set ( &key (max-index 1000))
    (apply #'append (loop :for i :from 2 :to max-index
			  :collect (diagonal-index i))))

(defun m-mk (lst)
  (make-instance 'math/arr-matr:<matrix> :initial-contents lst))

(defun make-matrix-m-1xm (mm)
  "Добавляет к матрице mm такое количество строк, чтобы их число
 стало на единицу меньше столбцов. Возвращет новую матрицу.
 Заполняет главную диагональ новой матрицы единицами."
  (let ((rez 
	  (make-instance
	   'math/arr-matr:<matrix>
	   :dimensions (list (1- (math:cols mm)) (math:cols mm))))
	(index nil)
	(skiped-rows 0))
    (loop :for r :from 0 :below (math:rows rez) :do
      (block cols
	(loop :for c :from r :below (math:cols rez) :do
	  (if (>= (- r skiped-rows ) (math:rows mm))
	      (progn
		(setf (math/arr-matr:mref rez r r) 1)
		(push r index)
		(return-from cols))
	      (if (= 0 (math/arr-matr:mref mm (- r skiped-rows) r))
		  (progn
		    (incf skiped-rows)
		    (push r index)
		    (setf (math/arr-matr:mref rez r r) 1)
		    (return-from cols))
		  (setf (math/arr-matr:mref rez r c)
			(math/arr-matr:mref mm (- r skiped-rows) c)))))))
    (values rez (remove-duplicates index))))

(defun set-last-col-values (m1 rows vals)
  (loop :for r :in rows
	:for v :in vals :do
	  (setf (math/arr-matr:mref m1 r (1- (math:cols m1))) v))
  m1)

(defmethod matr-col-row ((matr math/arr-matr:<matrix>))
  (multiple-value-bind (mm rows) (make-matrix-m-1xm matr)
    (let ((lay-iter (make-layer-iterator (length rows))))
      (loop :for i :from 0 :to (expt 1000 (length rows)) :do
	(let ((koeff
		(math:row (math/ls-gauss:solve-linear-system-gauss-backward-run 
			   (set-last-col-values mm rows (funcall lay-iter))) 
			  0)))
	  (when (every #'(lambda (el) (and (integerp el) (plusp el)))
		       koeff)
	    (return koeff)))))))

(defun equation-koeffitients (equation)
  (let ((matr (math/ls-gauss:convert-to-triangular
	       (m-mk
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
;;;; (format t "~S~%" matr)
    (matr-col-row matr)))

(defmethod culc-koeffitients ((reac <reaction>))
  (mapcar #'(lambda (moles sp) (setf (moles-number sp) moles))
	    (equation-koeffitients 
	     (append (mapcar #'elements (reaction-reactants reac))
		     (mapcar #'elements (reaction-products  reac))))
	    (append (reaction-reactants reac)(reaction-products reac)))
  reac)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; termo.lisp

;;;; make-instance 



(export 'make-instance-component )
(defun make-instance-component (component-name fraction &optional (fraction-type :mole))
"@b(Описание:) функция @b(make-instance-component) возвращает компонент
газовой смеси, заданной мольными или массовыми долями. По умолчанию
поределяется через мольную долю.

@b(Переменые:)
@begin(list)
@item(component-name - имя компонента;)
@item(fraction - доля компонента мольная или массовая;)
@item(fraction-type - тип доли. :mole задает мольную долю; :mass - массовую.
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

(export 'make-instance-composition )
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

(defgeneric dump (reference stream)
  (:documentation "@b(Описание:) метод @b(dump) сбравывает символьное
 представление reference в символьный поток stream."))

(defgeneric adapt-mole-fractions (reference)
  (:documentation "@b(Описание:) метод @b(adapt-mass-fractions)
выполняет подгонку состава смеси, заданной ммольными долями."))

(defgeneric  adapt-mass-fractions (reference)
  (:documentation "@b(Описание:) метод @b(adapt-mass-fractions)
выполняет подгонку состава смеси, заданной массовыми долями."))

(defgeneric insert (obj collection)
  (:documentation "@b(Описание:) обобщенная_функция @b(insert)
вставляет объект obj в коллекцию collection. "))

(defgeneric combustion-reaction (species)
  (:documentation "@b(Описание:) обобщенная_функция @b(combustion-reaction)"))

(defgeneric relativ-oxigen-mass-for-burning (species)
 (:documentation "@b(Описание:) обобщенная_функция
 @b(relativ-oxigen-mass-for-burning) возвращает количество килограмм
 кислорода (кг), необходимого для сжигания одного килограмма
 топлива."))

(defgeneric relativ-air-mass-for-burning  (species)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(relativ-air-mass-for-burning)
 возвращает количество килограмм воздуха (кг), необходимого для
 сжигания одного килограмма топлива. "))

(defgeneric wobber-hight (species)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(wobber-hight) возвращает число
 Воббе высшее относительное (по воздуху)."))

(defgeneric wobber-low (species)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(wobber-low) возвращает число
 Воббе низшее относительное (по воздуху). "))

(defgeneric thermal-effect (species)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(thermal-effect) возвращает
 тепловой эффект при создании вещества или при химической реакции. "))

(defgeneric Q-work-low (species)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(Q-work-low) возвращает низшую
 теплотворную способность топлива кДж/кг. "))

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
	     (composition-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-isobaric-heat-capacity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-isobaric-heat-capacity ((x <sp-rec>) temperature)
"Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]"
  (multiple-value-bind (a1 a2 a3 a4 a5 a6 a7)  (values-list (<sp-rec>-coefficients x))
    (* gases/const:+rμ+ (Cp/R-new temperature a1 a2 a3 a4 a5 a6 a7))))

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
	     (composition-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    molar-isochoric-heat-capacity                                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-isochoric-heat-capacity ((x <sp-rec>) temperature)
"Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
  (- (molar-isobaric-heat-capacity x temperature) gases/const:+Rμ+))
(defmethod molar-isochoric-heat-capacity ((x <sp>) temperature)
"Возвращает мольную изохорную теплоемкость muCv, [kJ/(mol*K)]"
  (- (molar-isobaric-heat-capacity x temperature) gases/const:+Rμ+))
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
	     (composition-components x))
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
       (* gases/const:+Rμ+ temperature (H/RT-new temperature a1 a2 a3 a4 a5 a6 a7 a8))))

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
	     (composition-components x))
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
    (* gases/const:+Rμ+ (S/R-new temperature a1 a2 a3 a4 a5 a6 a7 a9))))

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
	     (composition-components x))
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

(export 'molar-fraction-summ )
(defmethod molar-fraction-summ ((x <composition>))
"Возвращает сумму мольных долей смеси газов <composition>.
Значение должно равняться единице."
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (mole-fraction value) rez))
	     (composition-components x))
    (apply #'+ rez)))

(export 'mass-fraction-summ )
(defmethod mass-fraction-summ ((x <composition>))
"Возвращает сумму мольных долей смеси газов <composition>.
Значение должно равняться единице."
  (let ((rez nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (mass-fraction value) rez))
	     (composition-components x))
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mix-composition ((cmp-1 <composition>) (mfr-1 number)
			    (cmp-2 <composition>) (mfr-2 number))
"@b(Описание:) метод @b(mix-composition) возвращает список, состоящий из
компонентного состава, выраженного в мольных долях, и массового расхода.
"
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
				   (/ (+ (* (mass-fraction el-1) mfr-1)
					 (* (mass-fraction el-2) mfr-2))
				      (+ mfr-1 mfr-2))
				   :species (get-sp el))))
	     ((and el-1 (null el-2))
	      (setf (gethash el (composition-components cmp))
		    (make-instance '<component>
				   :mass-fraction
				   (/ (+ (* (mass-fraction el-1) mfr-1)
					 0.0)
				      (+ mfr-1 mfr-2))
				   :species (get-sp el)))
	      )
	     ((and el-2 (null el-1))
	      (setf (gethash el (composition-components cmp))
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

(export 'culc-mass-fractions )
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
     (composition-components cmp))
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
	     (composition-components x))
    (apply #'+ rez)))

(export 'culc-molar-fractions )
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
     (composition-components cmp))
    cmp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'check-mole-fraction )
(defmethod check-mole-fraction ((cmp <composition>))
"Проверка правильности задания мольных долей."
  (math/arr-matr:semi-equal (molar-fraction-summ cmp) 1.0))
(export 'check-mass-fraction )
(defmethod check-mass-fraction ((cmp <composition>))
"Проверка правильности задания массовых долей."
  (math/arr-matr:semi-equal (mass-fraction-summ cmp) 1.0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'reference )
(defmethod reference ((key string) (cmp <composition>))
"Получает ссылку на элемент, находящийся в конлейнере по ключу."
  (gethash key (composition-components cmp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'elemental-mass-fraction )
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
     (composition-components cmp))
    (values-list
     (reduce
      #'(lambda (x y)
	  (multiple-value-list 
	   (mix-composition (first x) (second x) (first y) (second y))))
      rez
      :initial-value (list (make-instance '<composition>) 0.0)))))

(export 'elemental-mass-fraction )
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
	   (<sp>-chemical-formula (species ref)))))
    (list cmp (mass-fraction ref))))

(export 'elemental-mass-fraction )
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
	   (<sp>-chemical-formula ref))))
    (list cmp 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'elements )
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

(export 'molar-mass )
(defmethod molar-mass ((rt <reactant>))
  (* (moles-number rt) (<sp>-molar-mass (species rt))))

(export 'molar-mass )
(defmethod molar-mass ((pt <product>))
  (* (moles-number pt) (<sp>-molar-mass (species pt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'thermal-effect )
(defmethod thermal-effect ((rt <reactant>))
"@b(Описание:) метод @b(thermal-effect) возвращает
 тепловой эффект при реагировании реактанта.
"
  (* -1
     (moles-number rt)
     (<sp>-heat-formation (species rt))))

(export 'thermal-effect )
(defmethod thermal-effect ((rt <product>))
"@b(Описание:) метод @b(thermal-effect) возвращает
 тепловой эффект получении продукта химической реакции.
"
  (* (moles-number rt)
     (<sp>-heat-formation (species rt))))

(export 'thermal-effect )
(defmethod thermal-effect ((reac <reaction>))
"@b(Описание:) метод @b(thermal-effect) возвращает
 тепловой эффект химической реакции.
"
  (apply #'+
   (mapcar #'thermal-effect
   (append (reaction-reactants reac) (reaction-products reac)))))
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

(export 'print-table )
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
  (setf (gethash (<sp>-name (species c)) (composition-components cmp)) c)
  cmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; adapt

(export 'adapt-mole-fractions )
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
(export ' adapt-mass-fractions )
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
;;;; combustion-reaction

(export 'combustion-reaction )
(defmethod combustion-reaction ((sp <sp>))
"@b(Описание:) метод @b(combustion-reaction) возвращает реакцию горения
компонента в кислороде.
В хештаблице *not-combasted-sp* сохраняются негорючие компоненты.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (combustion-reaction (get-sp \"CO\")) => 2*CO + 1*O2 => 2*CO2
 (combustion-reaction (get-sp \"N2\")) => 1*N2 => 1*N2
 (combustion-reaction (get-sp \"O2\")) => 1*O2 => 1*O2
@end(code)
"
  (let ((cmp (first (elemental-mass-fraction sp)))
	(good-combasted nil)
	(combasted-elem '("H" "C" "S"))
	(good-fuel      t)
	(fuel-elem '("H" "C" "S" "O" "N"))
	(reactants nil)
	(products  nil))
    (block not-combastor-sp
      (when (gethash (<sp>-name sp) *not-combasted-sp*)
	(return-from combustion-reaction
	  (make-instance 'gases:<reaction>
			 :reactant-names (list (<sp>-name sp))
			 :product-names  (list (<sp>-name sp))))))
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
	  (push (<sp>-name sp) reactants))
	(block compose-products
	  (when (reference "N" cmp) (push "N2" products ))
	  (when (reference "S" cmp) (push "SO2" products ))
	  (when (reference "C" cmp) (push "CO2" products ))
	  (when (reference "H" cmp) (push "H2O" products )))
	(make-instance '<reaction>
		       :reactant-names reactants
		       :product-names products)))))

(export 'combustion-reaction )
(defmethod combustion-reaction ((cmp <component>))
  (combustion-reaction (species cmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; relativ-oxigen-mass-for-burning

(export 'relativ-oxigen-mass-for-burning )
(defmethod relativ-oxigen-mass-for-burning ((sp <sp>))
  (let ((reactants (reaction-reactants (combustion-reaction sp))))
    (/ (molar-mass (second reactants)) (molar-mass (first reactants)))))

(export 'relativ-oxigen-mass-for-burning )
(defmethod relativ-oxigen-mass-for-burning ((cmp <component>))
  (let ((reactants (reaction-reactants (combustion-reaction cmp))))
    (* (mass-fraction cmp)
       (/ (molar-mass (second reactants)) (molar-mass (first reactants))))))

(export 'relativ-oxigen-mass-for-burning )
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
;;;; Q-work-low

(export 'Q-work-low )
(defmethod Q-work-low ((sp <sp>))
"@b(Описание:) метод @b(Q-work-low) 
"
  (let* ((reac-combustion (combustion-reaction sp))
	 (reactants (reaction-reactants reac-combustion))
	 (fuel   (first reactants)))
    (/ (thermal-effect reac-combustion)
       (moles-number fuel)
       (molar-mass (species fuel)))))

(export 'Q-work-low )
(defmethod Q-work-low ((c-t <component>))
"@b(Описание:) метод @b(Q-work-low)
"
  (* (mass-fraction c-t)
     (Q-work-low (species c-t))))

(export 'Q-work-low )
(defmethod Q-work-low ((c-n <composition>))
"@b(Описание:) метод @b(Q-work-low)
"
  (let ((rez 0.0))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (setf rez (+ rez (Q-work-low value))))
	     (composition-components c-n))
    rez))

(defmethod Q-volume-low ((sp <sp>) pressure temperature)
  (* (Q-work-low sp) (density sp pressure temperature)))

(defmethod Q-volume-low ((c-t <component>) pressure temperature)
  (* (Q-work-low c-t) (density c-t pressure temperature)))

(defmethod Q-volume-low ((c-n <composition>) pressure temperature)
  (* (Q-work-low c-n) (density c-n pressure temperature)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod density ((sp <sp>) pressure temperature)
  (/ (* (molar-mass sp) pressure)
     (* gases/const:+Rμ+ temperature)
     1000))

(defmethod density ((c-t <component>) pressure temperature)
  (/ (* (molar-mass c-t) pressure)
     (* gases/const:+Rμ+ temperature)
     1000))

(defmethod density ((c-n <composition>) pressure temperature)
  (/ (* (molar-mass c-n) pressure)
     (* gases/const:+Rμ+ temperature)
     1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;burning.lisp

(defparameter *O2*
  (make-instance-composition
   `(("O2" ,(/ 100. 100)))))

(defparameter *Air*
  (make-instance-composition
   `(("N2" ,(/ 78.084 100))
     ("O2" ,(/ 20.9476 100))
     ("Ar" ,(/ .9365 100))
     ("CO2",(/ .0319 100)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; relativ-air-mass-for-burning

(export 'relativ-air-mass-for-burning )
(defmethod relativ-air-mass-for-burning ((sp <sp>))
  (/ (relativ-oxigen-mass-for-burning sp)
     (mass-fraction (reference "O2" *air*))))

(export 'relativ-air-mass-for-burning )
(defmethod relativ-air-mass-for-burning ((cmp <component>))
  (/ (relativ-oxigen-mass-for-burning cmp)
     (mass-fraction (reference "O2" *air*))))

(export 'relativ-air-mass-for-burning )
(defmethod relativ-air-mass-for-burning ((cmp <composition>))
  (/ (relativ-oxigen-mass-for-burning cmp)
     (mass-fraction (reference "O2" *air*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Число Воббе низшее

(defmethod wobber-low ((sp <sp>))
  (let ((ρ-f (density sp gases/const:+p-normal+ gases/const:+t-normal+))
	(ρ-a (density *air* gases/const:+p-normal+ gases/const:+t-normal+)))
    (/ (* (Q-work-low sp) ρ-f) (sqrt (/ ρ-f ρ-a)))))

(defmethod wobber-low ((c-t <component>))
  (error "Not defined")
  )

(defmethod wobber-low ((c-n <composition>))
  (let ((ρ-f (density c-n gases/const:+p-normal+ gases/const:+t-normal+))
	(ρ-a (density *air* gases/const:+p-normal+ gases/const:+t-normal+)))
    (/ (* (Q-work-low c-n) ρ-f) (sqrt (/ ρ-f ρ-a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Число Воббе высшее

;;;; Число Воббе высшее
(defmethod wobber-hight ((sp <sp>))
    (error "Not defined yet")  
)

(defmethod wobber-hight ((c-t <component>))
  (error "Not defined")
  )

(defmethod wobber-hight ((c-n <composition>))
  (error "Not defined")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; select.lisp

(export 'q-of )
(defmacro q-of (elem func quan)
  `(find-if
    #'(lambda (el)
	(and (string= (first el) ,elem)
	     (,func (second el) ,quan)))
    formula))

(export 'find-atoms )
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

(export 'find-by-atoms )
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
