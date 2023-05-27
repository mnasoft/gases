;;;; /src/reac/reac.lisp

(defpackage gases/reac
  (:use cl gases/const gases/db gases/core)
  (:export molar-mass
           combustion-reaction 
           relativ-oxigen-mass-for-burning 
           relativ-air-mass-for-burning  
           wobber-hight 
           wobber-low 
           thermal-effect 
           Q-work-low)
  
  (:export make-instance-component
           make-instance-composition
           )
  (:export species)
  (:export <product>)
  (:export <reactant>)
  (:export <reaction>
           <reaction>-reactants
           <reaction>-products
           )
  (:export elements
           moles-number
           culc-molar-fractions
           ))

(in-package gases/reac)

(defclass <reactant> ()
  ((species :accessor species :initarg :species
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

(defclass <product> ()
  ((species :accessor species :initarg :species
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

(defclass <reaction> ()
  ((reactants :accessor <reaction>-reactants :initform nil
	      :documentation "Список реактантов химической реакции.")
   (products  :accessor <reaction>-products :initform nil
   	      :documentation "Список реактантов химической реакции."))
  (:documentation
   "Представляет продукт химической реакции."))

(defmethod initialize-instance :after ((react <reaction>)
				       &key (reactant-names nil)
					 (product-names nil))
  (when reactant-names
    (setf (<reaction>-reactants react)
	  (mapcar #'(lambda (el) (make-instance '<reactant> :species (get-sp el)))
	   reactant-names)))
  (when product-names
    (setf (<reaction>-products react)
	  (mapcar #'(lambda (el) (make-instance '<product> :species (get-sp el)))
		  product-names)))
  (culc-koeffitients react)
  react)

(defmethod print-object ((reac <reaction>) s)
  (format s "~{~A~^ + ~} => ~{~A~^ + ~}"
	  (<reaction>-reactants reac) (<reaction>-products  reac)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (make-instance 'math/matr:<matrix> :initial-contents lst))

(defun make-matrix-m-1xm (mm)
  "Добавляет к матрице mm такое количество строк, чтобы их число
 стало на единицу меньше столбцов. Возвращет новую матрицу.
 Заполняет главную диагональ новой матрицы единицами."
  (let ((rez 
	  (make-instance
	   'math/matr:<matrix>
	   :dimensions (list (1- (math/matr:cols mm)) (math/matr:cols mm))))
	(index nil)
	(skiped-rows 0))
    (loop :for r :from 0 :below (math/matr:rows rez) :do
      (block cols
	(loop :for c :from r :below (math/matr:cols rez) :do
	  (if (>= (- r skiped-rows ) (math/matr:rows mm))
	      (progn
		(setf (math/matr:mref rez r r) 1)
		(push r index)
		(return-from cols))
	      (if (= 0 (math/matr:mref mm (- r skiped-rows) r))
		  (progn
		    (incf skiped-rows)
		    (push r index)
		    (setf (math/matr:mref rez r r) 1)
		    (return-from cols))
		  (setf (math/matr:mref rez r c)
			(math/matr:mref mm (- r skiped-rows) c)))))))
    (values rez (remove-duplicates index))))

(defun set-last-col-values (m1 rows vals)
  (loop :for r :in rows
	:for v :in vals :do
	  (setf (math/matr:mref m1 r (1- (math/matr:cols m1))) v))
  m1)

(defmethod matr-col-row ((matr math/matr:<matrix>))
  (multiple-value-bind (mm rows) (make-matrix-m-1xm matr)
    (let ((lay-iter (make-layer-iterator (length rows))))
      (loop :for i :from 0 :to (expt 1000 (length rows)) :do
	(let ((koeff
		(math/matr:row
                 (math/ls-gauss:solve-x 
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
	     (append (mapcar #'elements (<reaction>-reactants reac))
		     (mapcar #'elements (<reaction>-products  reac))))
	    (append (<reaction>-reactants reac)(<reaction>-products reac)))
  reac)

(defmethod molar-mass ((rt <reactant>))
  (* (moles-number rt) (<sp>-molar-mass (species rt))))

(defmethod molar-mass ((pt <product>))
  (* (moles-number pt) (<sp>-molar-mass (species pt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod thermal-effect ((rt <reactant>))
"@b(Описание:) метод @b(thermal-effect) возвращает
 тепловой эффект при реагировании реактанта.
"
  (* -1
     (moles-number rt)
     (<sp>-heat-formation (species rt))))

(defmethod thermal-effect ((rt <product>))
"@b(Описание:) метод @b(thermal-effect) возвращает
 тепловой эффект получении продукта химической реакции.
"
  (* (moles-number rt)
     (<sp>-heat-formation (species rt))))

(defmethod thermal-effect ((reac <reaction>))
"@b(Описание:) метод @b(thermal-effect) возвращает
 тепловой эффект химической реакции.
"
  (apply #'+
   (mapcar #'thermal-effect
           (append (<reaction>-reactants reac) (<reaction>-products reac)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; combustion-reaction

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	  (make-instance '<reaction>
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
       (<composition>-components cmp))
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

(defmethod combustion-reaction ((cmp <component>))
  (combustion-reaction (species cmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; relativ-oxigen-mass-for-burning

(defmethod relativ-oxigen-mass-for-burning ((sp <sp>))
  (let ((reactants (<reaction>-reactants (combustion-reaction sp))))
    (/ (molar-mass (second reactants)) (molar-mass (first reactants)))))

(defmethod relativ-oxigen-mass-for-burning ((cmp <component>))
  (let ((reactants (<reaction>-reactants (combustion-reaction cmp))))
    (* (mass-fraction cmp)
       (/ (molar-mass (second reactants)) (molar-mass (first reactants))))))

(defmethod relativ-oxigen-mass-for-burning ((cmp <composition>))
  (let ((components (<composition>-components cmp))
	(rez nil))
    (maphash
     #'(lambda (key value)
	 (declare (ignore key))
	 (push (relativ-oxigen-mass-for-burning value) rez))
     components)
    (apply #'+ rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Q-work-low

(defmethod Q-work-low ((sp <sp>))
"@b(Описание:) метод @b(Q-work-low) 
"
  (let* ((reac-combustion (combustion-reaction sp))
	 (reactants (<reaction>-reactants reac-combustion))
	 (fuel   (first reactants)))
    (/ (thermal-effect reac-combustion)
       (moles-number fuel)
       (molar-mass (species fuel)))))

(defmethod Q-work-low ((c-t <component>))
"@b(Описание:) метод @b(Q-work-low)
"
  (* (mass-fraction c-t)
     (Q-work-low (species c-t))))

(defmethod Q-work-low ((c-n <composition>))
"@b(Описание:) метод @b(Q-work-low)
"
  (let ((rez 0.0))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (setf rez (+ rez (Q-work-low value))))
	     (<composition>-components c-n))
    rez))

(defmethod Q-volume-low ((sp <sp>) pressure temperature)
  (* (Q-work-low sp) (density sp pressure temperature)))

(defmethod Q-volume-low ((c-t <component>) pressure temperature)
  (* (Q-work-low c-t) (density c-t pressure temperature)))

(defmethod Q-volume-low ((c-n <composition>) pressure temperature)
  (* (Q-work-low c-n) (density c-n pressure temperature)))

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

(defmethod relativ-air-mass-for-burning ((sp <sp>))
  (/ (relativ-oxigen-mass-for-burning sp)
     (mass-fraction (reference "O2" *air*))))

(defmethod relativ-air-mass-for-burning ((cmp <component>))
  (/ (relativ-oxigen-mass-for-burning cmp)
     (mass-fraction (reference "O2" *air*))))

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
