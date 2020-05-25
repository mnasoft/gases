;;;; classes.lisp

(in-package :gases)

(annot:enable-annot-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.class:export-class
(defclass <molecule> nil
  ((molecule-name-ru
    :accessor molecule-name-ru :initarg :name-ru
    :initform ""
    :documentation
    "Обозначение русскоязычное")
   (molecule-name-en
    :accessor molecule-name-en
    :initarg :name-en
    :initform "" :documentation
    "Обозначение англоязычное")
   (molecule-name-en-short
    :accessor molecule-name-en-short :initarg
    :name-en-short :initform ""
    :documentation
    "Короткое англоязычное обозначение")
   (molecule-smile
    :accessor molecule-smile :initarg :smile :initform "" :documentation "Smile")
   (molecule-mass
    :accessor molecule-mass :initarg :mass :initform ""
    :documentation "Молекулярная масса кг/моль")
   (molecule-μcp-a-b-c
    :accessor molecule-μcp-a-b-c :initarg :μcp-a-b-c
    :initform ""
    :documentation
    "Коэффициенты для расчета мольной теплоемкости ккал/(моль*К). 
Данные взяты из файла ./doc/111.jpg (см. мультитехнический справочник Интернет).")
   (molecule-formula
    :accessor molecule-formula :initarg :formula
    :initform "" :documentation "Химическая формула")
   (molecule-note :accessor molecule-note :initarg :note :initform ""
                  :documentation "Примечание"))
  (:documentation "Представляет молекулу вещества."))

(defmethod print-object :before ((x <molecule>) s)
  (format s " #<molecule>(~S ~S ~S ~S ~S ~S"
	  (molecule-name-en-short x)
	  (molecule-name-ru x)
	  (molecule-μcp-a-b-c x)
	  (molecule-mass x)
	  (molecule-formula x)
	  (molecule-note x)))

(defmethod print-object         ((x <molecule>) s) (format s "" ))

(defmethod print-object :after  ((x <molecule>) s) (format s ")" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.class:export-class
(defclass <sp-rec> nil
  ((sp-rec-temperature-range
    :accessor sp-rec-temperature-range
    :initarg :temperature-range
    :documentation
    "Temperature range (cols 2-21, 2x 10.3f). 
The minimum and maximum bounds for the current temperature interval.
Units, K.")
   (sp-rec-number-coeff
    :accessor sp-rec-number-coeff :initarg
    :number-coeff :initform 7 :documentation
    "Number of coefficients (col 23, int). 
This is always 7 in this data (though the database format supports 8,
see section Redundancy).")
   (sp-rec-polynomial-exponents
    :accessor sp-rec-polynomial-exponents
    :initarg :polynomial-exponents
    :initform '(-2 -1 0 1 2 3 4)
    :documentation
    "Polynomial exponents (cols 24-63, 8x 5.1f). 
These are always [-2, -1, 0, 1, 2, 3, 4] in this data.")
   (sp-rec-h_298.15-h-0
    :accessor sp-rec-h_298.15-h-0 :initarg
    :h_298.15-h-0 :documentation
    "{H(298.15) - H(0)} (cols 66-80, 15.3f). 
This is the difference between the heat of formation at the enthalpy at T = 0 K.")
   (sp-rec-coefficients
    :accessor sp-rec-coefficients :initarg
    :coefficients :initform '(0 0 0 0 0 0 0)
    :documentation
    "Coefficients 1-5 (cols 1-80, 5x 16.8f). 
Coefficients 6-8 (cols 1-48, 3x 16.8f). 
The 8th is not used in this data (see section Redundancy).")
   (sp-rec-integration-constants
    :accessor sp-rec-integration-constants
    :initarg :integration-constants
    :initform '(0 0)
    :documentation
    "Integration constants (cols 49-80, 2x 16.8f). 
Used in evaluation of enthalpy and temperature-dependent 
component of entropy, respectively."))
  (:documentation
   "Представляет данные для расчета теплоемкости, энтальпии и 
энторопии в определенном диапазоне температур."))

(defmethod print-object :before ((x <sp-rec>) s)
  (format s
	  "#<sp-rec>(
 sp-rec-temperature-range=~S
 sp-rec-number-coeff=~S
 sp-rec-polynomial-exponents=~S
 sp-rec-H_298.15-H-0=~S
 sp-rec-coefficients=~S
 sp-rec-integration-constants=~S~%"
	  (sp-rec-temperature-range x)
	  (sp-rec-number-coeff x)
	  (sp-rec-polynomial-exponents x)
	  (sp-rec-H_298.15-H-0 x)
	  (sp-rec-coefficients x)
	  (sp-rec-integration-constants x)))

(defmethod print-object         ((x <sp-rec>) s) (format s "" ))

(defmethod print-object :after  ((x <sp-rec>) s) (format s ")" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.class:export-class
(defclass <sp> nil
  ((sp-name
    :accessor sp-name :initarg :name :initform ""
    :documentation
    "Species name/formula (cols 1-15, 15str). 
This serves as an ID. Note that 'l' is represented by L and 
condensed phases designated as α, β, γ or δ are renamed
a, b, c or d due to ASCII limitations.")
   (sp-comments
    :accessor sp-comments :initarg :comments :initform ""
    :documentation
    "Comments (cols 16-80, 65str). These include references in the 
format of author, year or page and date in the case of TRC tables. 
When heat of formation is taken from a separate reference, this is 
included as Hf:<ref>. Reference elements or sp used for heat of 
formation calculations are indicated by Ref-Elm or Ref-Sp.")
   (sp-number-temperature-intervals
    :accessor sp-number-temperature-intervals
    :initarg :number-temperature-intervals
    :initform 0
    :documentation
    "Number of temperature intervals (col 2, 2int).")
   (sp-reference-date-code
    :accessor sp-reference-date-code :initarg
    :reference-date-code :initform ""
    :documentation
    "Reference-Date code (cols 4-9, 6str). This includes a character 
 indicating a general reference followed by a date (e.g. g indicates 
 that NASA Glenn was the source of significant work in deriving the data 
 and 10/96 indicates the month/year).")
   (sp-chemical-formula
    :accessor sp-chemical-formula :initarg
    :chemical-formula :initform "" :documentation
    "Chemical formula (cols 11-50, 2str + 6.2f). This is a set of 5 element/atom, 
 number pairs. In the vast majority of cases the numbers are integers but
 in some cases they are non-integer, so floats are used.")
   (sp-phase
    :accessor sp-phase :initarg :phase :initform ""
    :documentation
    "Phase (col 52, int). Zero for gas, nonzero for condensed phases.")
   (sp-molar-mass
    :accessor sp-molar-mass :initarg :molar-mass
    :initform "" :documentation
    "Molar mass (cols 53-65, 13.5f). Originally labelled molecular weight 
 (in units g/mol).")
   (sp-heat-formation
    :accessor sp-heat-formation :initarg :heat-formation :initform ""
    :documentation
    "Heat of formation (cols 66-80, 13.5f). 
 In the case of condensed species this is actually an assigned enthalpy 
 (equivalent to the heat of formation at 298.15 K). Units J/mol.")
   (sp-reccords
    :accessor sp-reccords :initarg :reccords :initform ""
    :documentation
    "Список из нескольких элементов класса sp-rec"))
  (:documentation
   "Представляет молекулу вещества (Species name/Formula). 
 Данные для элементов взяты из базы данных NASA 
 (см. https://www.grc.nasa.gov/www/CEAWeb/)"))

(defmethod print-object :before ((x <sp>) s)
  (format s
	  "#<sp>(
 sp-name=~S
 sp-comments=~S
 sp-number-temperature-intervals=~S
 sp-reference-date-code=~S
 sp-chemical-formula=~S
 sp-phase=~S
 sp-molar-mass=~S
 sp-heat-formation=~S
 sp-reccords=~S~%"
	  (sp-name x)
	  (sp-comments x)
	  (sp-number-temperature-intervals x)
	  (sp-reference-date-code x)
	  (sp-chemical-formula x)
	  (sp-phase x)
	  (sp-molar-mass x)
	  (sp-heat-formation x)
	  (sp-reccords x)))

(defmethod print-object         ((x <sp>) s) (format s "" ))

(defmethod print-object :after  ((x <sp>) s) (format s ")" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.class:export-class
(defclass <component> nil
  ((component-species :accessor component-species :initarg :species
		      :documentation
		      "Должен содержать объект типа <sp>.")
   (component-mole-fraction :accessor component-mole-fraction :initarg
			    :mole-fraction :initform 0.0 :documentation
			    "Содежит мольную долю компонета в смеси.")
   (component-mass-fraction :accessor component-mass-fraction :initarg :mass-fraction :initform 0.0
			    :documentation
			    "Содежит массовую долю компонета в смеси."))
  (:documentation
   "Представляет компонент смеси, заданной мольными долями."))

(defmethod print-object :before ((x <component>) s)
  (format s
	  "#<component>(name=~S mole-fraction=~S mass-fraction=~S"
	  (sp-name (component-species x)) (component-mole-fraction x) (component-mass-fraction x)))

(defmethod print-object         ((x <component>) s) (format s "" ))

(defmethod print-object :after  ((x <component>) s) (format s ")" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.class:export-class
(defclass <composition> nil
  ((composition-components :accessor composition-components :initarg :components
			   :documentation
			   "Содержит список компонентов. 
Элементами этого списка д.б. данные типа <component>"))

  (:documentation
   "Представляет смесь, состоящую из объектов класса <component>."))

@annot.doc:doc
"Проверка на то, что заданные компоненты имеются в базе данных."
(defun check-spices-exist-in-db (lst)
  (reduce
   #'(lambda (el1 el2)
       (and el1 (get-sp (first el2))))
   lst
   :initial-value t))

@annot.doc:doc
"Проверка на неповторяемость компопнентов, передаваемых в конструктор."
(defun check-spices-is-unique (lst)
  (= (length lst)
     (length
      (remove-duplicates
       (mapcar #'first lst)
       :test #'string=))))


(defmethod initialize-instance :after ((cmp <composition>)
				       &key (components (make-hash-table :test #'equal)))
  (setf (composition-components cmp) components)
  )

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

@annot.class:export-class
(defclass <reactant> ()
  ((reactant-species :accessor reactant-species :initarg :species
		      :documentation
		      "Должен содержать объект типа <sp>.")
   (moles-number :accessor moles-number :initarg :mole :initform nil
		 :documentation "Количество молей реактанта, участвующих в химической реакции."))
  (:documentation
   "Представляет реактант химической реакции."))

(defmethod print-object ((rt <reactant>) s)
  (format s "~A*~A" (moles-number rt) (sp-name (reactant-species rt))))

(defmethod elements ((rt <reactant>))
  (elements (reactant-species rt)))

(defclass <product> ()
  ((product-species :accessor product-species :initarg :species
		      :documentation
		      "Должен содержать объект типа <sp>.")
   (moles-number :accessor moles-number :initarg :mole :initform nil
		 :documentation "Количество молей продукта, получаемого а результате химической реакции."))
  (:documentation
   "Представляет продукт химической реакции."))

(defmethod print-object ((pt <product>) s)
  (format s "~A*~A" (moles-number pt) (sp-name (product-species pt))))

(defmethod elements ((pt <product>))
  (labels ((minus (lst)
	     (mapcar
	      #'(lambda (el) (list (first el) (- (second el))))
	      lst)))
    (minus (elements (product-species pt)))))

@annot.class:export-class
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

@annot.doc:doc
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
(defun make-layer-iterator (vars)
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

@annot.doc:doc
  "@b(Описание:) функция @b(atoms) возвращает список элементов,
 из которых состоит молекула.

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

(defun make-linear-index-set ( &key (max-index 1000))
  (loop :for i :from 1 :to max-index :collect `(,i)))

(defun diagonal-index ( x )
  (loop :for i :from 1 :below x
	:collect (list i (- x i))))

(defun make-diagonal-index-set ( &key (max-index 1000))
    (apply #'append (loop :for i :from 2 :to max-index
			  :collect (diagonal-index i))))

(defun m-mk (lst)
  (make-instance 'math:<matrix> :initial-contents lst))

(defun make-matrix-m-1xm (mm)
  "Добавляет к матрице mm такое количество строк, чтобы их число
 стало на единицу меньше столбцов. Возвращет новую матрицу.
 Заполняет главную диагональ новой матрицы единицами."
  (let ((rez 
	  (make-instance
	   'math:<matrix>
	   :dimensions (list (1- (math:cols mm)) (math:cols mm))))
	(index nil)
	(skiped-rows 0))
    (loop :for r :from 0 :below (math:rows rez) :do
      (block cols
	(loop :for c :from r :below (math:cols rez) :do
	  (if (>= (- r skiped-rows ) (math:rows mm))
	      (progn
		(setf (math:mref rez r r) 1)
		(push r index)
		(return-from cols))
	      (if (= 0 (math:mref mm (- r skiped-rows) r))
		  (progn
		    (incf skiped-rows)
		    (push r index)
		    (setf (math:mref rez r r) 1)
		    (return-from cols))
		  (setf (math:mref rez r c)
			(math:mref mm (- r skiped-rows) c)))))))
    (values rez (remove-duplicates index))))

(defun set-last-col-values (m1 rows vals)
  (loop :for r :in rows
	:for v :in vals :do
	  (setf (math:mref m1 r (1- (math:cols m1))) v))
  m1)

(defmethod matr-col-row=2 ((matr math:<matrix>))
  (dolist (lst-of-1 (make-linear-index-set))
    (multiple-value-bind (mm rows) (make-matrix-m-1xm matr)
      (let ((koeff (math:row
		    (math:solve-linear-system-gauss-backward-run 
		     (set-last-col-values mm rows lst-of-1))
		    0)))
	(when (every
	       #'(lambda (el) (and (integerp el) (plusp el) ))
	       koeff)
	  (return koeff))))))

@annot.doc:doc
"Подбор решения расширенной однородной матрицых в целых коэффициентах с двумя произвольными."
(defmethod matr-col-row=3 ((matr math:<matrix>))
  (dolist (lst-of-2 (make-diagonal-index-set))
    (multiple-value-bind (mm rows) (make-matrix-m-1xm matr)
      (let ((koeff (math:row
		    (math:solve-linear-system-gauss-backward-run 
		     (set-last-col-values mm rows lst-of-2))
		    0)))
	(when (every
	       #'(lambda (el) (and (integerp el) (plusp el) ))
	       koeff)
	  (return koeff))))))

(defun equation-koeffitients (equation)
  (let ((matr (math:convert-to-triangular
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
;;;;    (format t "~S~%" matr)
    (cond
      ((= 2 (- (math:cols matr) (math:rows matr)))
       (matr-col-row=2 matr))
      ((= 3 (- (math:cols matr) (math:rows matr)))
       (matr-col-row=3 matr)))))

(defmethod culc-koeffitients ((reac <reaction>))
  (mapcar #'(lambda (moles sp) (setf (moles-number sp) moles))
	    (equation-koeffitients 
	     (append (mapcar #'elements (reaction-reactants reac))
		     (mapcar #'elements (reaction-products  reac))))
	    (append (reaction-reactants reac)(reaction-products reac)))
  reac)
