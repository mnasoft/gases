;;;; gases.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sp-rec ()
  ((sp-rec-temperature-range      :accessor sp-rec-temperature-range     :initarg :sp-rec-temperature-range     :initform nil                :documentation "Temperature range (cols 2-21, 2x 10.3f). The minimum and maximum bounds for the current temperature interval. Units, K.")
   (sp-rec-number-coeff           :accessor sp-rec-number-coeff          :initarg :sp-rec-number-coeff          :initform 7                  :documentation "Number of coefficients (col 23, int). This is always 7 in this data (though the database format supports 8, see section Redundancy).")
   (sp-rec-polynomial-exponents   :accessor sp-rec-polynomial-exponents  :initarg :sp-rec-polynomial-exponents  :initform '(-2 -1 0 1 2 3 4) :documentation "Polynomial exponents (cols 24-63, 8x 5.1f). These are always [-2, -1, 0, 1, 2, 3, 4] in this data.")
   (sp-rec-H_298.15-H-0           :accessor sp-rec-H_298.15-H-0          :initarg :sp-rec-H_298.15-H-0          :initform nil                :documentation "{H(298.15) - H(0)} (cols 66-80, 15.3f). This is the difference between the heat of formation at the enthalpy at T = 0 K.")
   (sp-rec-coefficients           :accessor sp-rec-coefficients          :initarg :sp-rec-coefficients          :initform '( 0  0 0 0 0 0 0) :documentation "Coefficients 1-5 (cols 1-80, 5x 16.8f). Coefficients 6-8 (cols 1-48, 3x 16.8f). The 8th is not used in this data (see section Redundancy).")
   (sp-rec-integration-constants  :accessor sp-rec-integration-constants :initarg :sp-rec-integration-constants :initform '( 0  0)           :documentation "Integration constants (cols 49-80, 2x 16.8f). Used in evaluation of enthalpy and temperature-dependent component of entropy, respectively."))
  (:documentation "Представляет данные для расчета теплоемкости, энтальпии и энторопии в определенном диапазоне температур."))

(defmethod print-object :before ((x sp-rec) s)
	   (format s
		   "#sp-rec~%(sp-rec-temperature-range=~S~% sp-rec-number-coeff=~S~% sp-rec-polynomial-exponents=~S~% sp-rec-H_298.15-H-0=~S~% sp-rec-coefficients=~S~% sp-rec-integration-constants=~S~%"
		   (sp-rec-temperature-range x) (sp-rec-number-coeff x) (sp-rec-polynomial-exponents x) (sp-rec-H_298.15-H-0 x) (sp-rec-coefficients x) (sp-rec-integration-constants x)))

(defmethod print-object         ((x sp-rec) s) (format s "" ))

(defmethod print-object :after  ((x sp-rec) s) (format s ")" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sp ()
  ((sp-name                         :accessor sp-name                         :initarg :sp-name                         :initform "" :documentation "Species name/formula (cols 1-15, 15str). This serves as an ID. Note that 'l' is represented by L and condensed phases designated as α, β, γ or δ are renamed a, b, c or d due to ASCII limitations.")
   (sp-comments                     :accessor sp-comments                     :initarg :sp-comments                     :initform "" :documentation "Comments (cols 16-80, 65str). These include references in the format of author, year or page and date in the case of TRC tables. When heat of formation is taken from a separate reference, this is included as Hf:<ref>. Reference elements or sp used for heat of formation calculations are indicated by Ref-Elm or Ref-Sp.")
   (sp-number-temperature-intervals :accessor sp-number-temperature-intervals :initarg :sp-number-temperature-intervals :initform 0  :documentation "Number of temperature intervals (col 2, 2int).")
   (sp-reference-date-code          :accessor sp-reference-date-code          :initarg :sp-reference-date-code          :initform "" :documentation "Reference-Date code (cols 4-9, 6str). This includes a character indicating a general reference followed by a date (e.g. g indicates that NASA Glenn was the source of significant work in deriving the data and 10/96 indicates the month/year).")
   (sp-chemical-formula             :accessor sp-chemical-formula             :initarg :sp-chemical-formula             :initform "" :documentation "Chemical formula (cols 11-50, 2str + 6.2f). This is a set of 5 element/atom, number pairs. In the vast majority of cases the numbers are integers but in some cases they are non-integer, so floats are used.")
   (sp-phase                        :accessor sp-phase                        :initarg :sp-phase                        :initform "" :documentation "Phase (col 52, int). Zero for gas, nonzero for condensed phases.")
   (sp-molar-mass                   :accessor sp-molar-mass                   :initarg :sp-molar-mass                   :initform "" :documentation "Molar mass (cols 53-65, 13.5f). Originally labelled molecular weight (in units g/mol).")
   (sp-heat-formation               :accessor sp-heat-formation               :initarg :sp-heat-formation               :initform "" :documentation "Heat of formation (cols 66-80, 13.5f). In the case of condensed species this is actually an assigned enthalpy (equivalent to the heat of formation at 298.15 K). Units J/mol.")
   (sp-reccords                     :accessor sp-reccords                     :initarg :sp-reccords                     :initform "" :documentation "Список из нескольких элементов класса sp-rec"))
  (:documentation "Представляет молекулу вещества. Species name/formula"))

(defmethod print-object :before ((x sp) s)
	   (format s
		   "#sp(sp-name=~S~% sp-comments=~S~% sp-number-temperature-intervals=~S~% sp-reference-date-code=~S~% sp-chemical-formula=~S~% sp-phase=~S~% sp-molar-mass=~S~% sp-heat-formation=~S~% sp-reccords=~S~%"
		   (sp-name x) (sp-comments x) (sp-number-temperature-intervals x) (sp-reference-date-code x) (sp-chemical-formula x) (sp-phase x) (sp-molar-mass x) (sp-heat-formation x) (sp-reccords x)))

(defmethod print-object         ((x sp) s) (format s "" ))

(defmethod print-object :after  ((x sp) s) (format s ")" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass component ()
  ((component-species       :accessor component-species       :initarg :component-species       :initform nil :documentation "Должен содержать объект типа sp.")
   (component-mole-fraction :accessor component-mole-fraction :initarg :component-mole-fraction :initform 0.0 :documentation "Содежит мольную долю компонета."))
  (:documentation "Представляет компонент смеси заданной мольными долями."))

(defmethod print-object :before ((x component) s)
	   (format s
		   "#component(component-species=~S~%component-mole-fraction=~S"
		   (component-species x) (component-mole-fraction x)))

(defmethod print-object         ((x component) s) (format s "" ))

(defmethod print-object :after  ((x component) s) (format s ")" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass composition ()
  ((composition-components :accessor composition-components :initarg :composition-components :initform nil :documentation "Содержит список компонентов.")))

(defmethod print-object :before ((x composition) s)
	   (format s
		   "#composition(composition-components=~S"
		   (composition-components x)))

(defmethod print-object         ((x composition) s) (format s "" ))

(defmethod print-object :after  ((x composition) s) (format s ")" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;make-instance

(defun make-instance-sp-rec (lst)
  (make-instance
   'sp-rec
   :sp-rec-temperature-range    (list (nth  0 lst) (nth  1 lst))
   :sp-rec-number-coeff         (nth  2 lst)
   :sp-rec-polynomial-exponents (subseq lst 3 (+ 3 (nth  2 lst)))
   :sp-rec-H_298.15-H-0         (nth  11 lst)
   :sp-rec-coefficients          (subseq lst 12 (+ 12 (nth  2 lst)))
   :sp-rec-integration-constants (list (nth  20 lst) (nth  21 lst))))


(defun make-instance-sp (lst)
  "Создает оъект sp "
  (make-instance
   'sp
   :sp-name (first lst)
   :sp-comments (second lst)
   :sp-number-temperature-intervals (third lst)
   :sp-reference-date-code (fourth lst)         
   :sp-chemical-formula (list (list (nth  4 lst) (nth  5 lst))
			      (list (nth  6 lst) (nth  7 lst))
			      (list (nth  8 lst) (nth  9 lst))
			      (list (nth 10 lst) (nth 11 lst))
			      (list (nth 12 lst) (nth 13 lst)))            
   :sp-phase                        (nth 14 lst)
   :sp-molar-mass                   (nth 15 lst)
   :sp-heat-formation               (nth 16 lst)
   :sp-reccords               (mapcar #'make-instance-sp-rec (car(last lst)))))


(defun make-instance-component (component-name mole-fraction)
  (make-instance 'component
		 :component-species (gethash component-name *sp-db*)
		 :component-mole-fraction mole-fraction))


(make-instance-component "N2" 0.78)

(make-instance-component "N2" 0.21)

(defun make-instance-composition (lst)
  (make-instance 'composition
		 :composition-components (mapcar
					  #'(lambda(el)
					      (make-instance-component (first el)(second el)))
					  lst)))

(make-instance-composition '(("N2" 0.78) ("O2" 0.20)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;read-formated-data

(defun read-string (str &optional (ft 's))
  (cond
    ((or (eq ft 'f) (eq ft 'e) (eq ft 'd) (eq ft 'g) (eq ft 'i))
     (if (string= "" str) 0.0 (read (make-string-input-stream str))))
    ((eq ft 's) str)
    (t (break "Format error!"))))

(defun read-record (ln-str-format str)
  (let ((st 0))
    (mapcar
     #'(lambda (el)
	 (let ((rez (string-trim " " (subseq str st (+ (second el) st)))))
	   (setf st (+ st (second el)))
	   (read-string rez (first el))))
     ln-str-format)))

(defun read-el-header (is str-1)
  (let* ((rec-1 '((s 18) (s 62)))
	 (rec-2 '((i  2) (s  8) (s  2) (f  6) (s 2) (f 6) (s 2) (f 6) (s 2) (f 6) (s 2) (f 6) (i 2) (f 13) (f 15)))
	 (rec-3 '((f 11) (f 11) (i  1) (f  5) (f 5) (f 5) (f 5) (f 5) (f 5) (f 5) (f 5) (f 17)))
	 (rec-4 '((f 16) (f 16) (f 16) (f 16) (f 16)))
	 (rec-5 '((f 16) (f 16) (f 16) (f 16) (f 16)))
	 (r-1 (read-record rec-1 str-1))
	 (r-2 (read-record rec-2 (read-line is)))
	 (n-row (first r-2))
	 (t-int nil))
    (dotimes (i n-row)
      (push 
       (append (read-record rec-3 (read-line is))
	       (read-record rec-4 (read-line is))
	       (read-record rec-5 (read-line is)))
       t-int))
    (append r-1 r-2 (list (reverse t-int)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clean-termo-inp ()
  "Выполняет очистку формата ввода данных от комментариев
Пример использования:
(clean-termo-inp)
"
  (with-open-file (os "~/quicklisp/local-projects/clisp/gases/gases/data/termo.inp.clean" :direction :output :if-exists :supersede)
    (with-open-file (is "~/quicklisp/local-projects/clisp/gases/gases/data/termo.inp" :direction :input)
      (do ((line (read-line is nil 'eof) (read-line is nil 'eof))
	   (str-format "~A"))
	  ((eql line 'eof))
	(unless
	    (or (string= "" (string-trim " " line))
		(string= "!" (subseq line 0 1))
		(string= "#" (subseq line 0 1))
		(string= "END PRODUCTS" (string-trim " " line))
		(string= "END REACTANTS" (string-trim " " line)))
	  (format os str-format line)
	  (setf str-format "~%~A"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-element-table()
  "Пример использования:
(make-element-table)
"
  (let ((rez nil)
	(rez-lst nil))
    (with-open-file (is "~/quicklisp/local-projects/clisp/gases/gases/data/termo.inp.clean" :direction :input)
      (read-line is nil 'eof)
      (read-line is nil 'eof)
      (do ((line (read-line is nil 'eof) (read-line is nil 'eof)))
	  ((eql line 'eof))
	(setf rez (read-el-header is line))
	(push rez rez-lst)))
    rez-lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sp-db* (make-hash-table :test #'equal))

(mapc #'(lambda (el)
	  (let ((sp-elem (make-instance-sp el)))
	    (setf (gethash (sp-name sp-elem) *sp-db*) sp-elem)))
      (make-element-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;methods

(defmethod molar-mass ((x sp))
  "Возвращает молекулярную массу, [g/mol]
Пример использования
(molar-mass (gethash \"N2\" *sp-db*)) => 28.0134
(molar-mass (gethash \"CH4\" *sp-db*)) => 16.04246
"
  (sp-molar-mass x))

(defmethod molar-isobaric-heat-capacity ((x sp) temperature)
  "Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]"
  nil
  )

(defmethod molar-isobaric-heat-capacity ((x sp-rec) temperature)
  "Возвращает мольную изобарную теплоемкость muCp, [J/(mol*K)]"
  nil
  )

(defgeneric molar-isochoric-heat-capacity (species temperature)
  (:documentation "Возвращает мольную изобарную теплоемкость 
- для класса species
- в зависимости от температуры (temperature), [K]"))

(defmethod molar-isochoric-heat-capacity ((x sp) temperature)
    "Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
    )

(defmethod molar-isochoric-heat-capacity ((x sp-rec) temperature)
    "Возвращает мольную изохорную теплоемкость muCv, [J/(mol*K)]"
    )

(molar-mass (gethash "N2" *sp-db*))
	    
(molar-mass (gethash "CH4" *sp-db*))

(defmethod muCv ((x sp) temperature)
  )

(defmethod adiabatic-index ((x sp) temperature)
  "Возвращает показатель адиабаты"
  
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


