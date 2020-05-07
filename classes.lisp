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
  ((component-species
    :accessor component-species :initarg :species
    :documentation
    "Должен содержать объект типа <sp>.")
   (component-mole-fraction
    :accessor component-mole-fraction :initarg
    :mole-fraction :initform 0.0 :documentation
    "Содежит мольную долю компонета в смеси."))
  (:documentation
   "Представляет компонент смеси, заданной мольными долями."))

(defmethod print-object :before ((x <component>) s)
  (format s
	  "#<component>(component-species=~S~%component-mole-fraction=~S"
	  (component-species x) (component-mole-fraction x)))

(defmethod print-object         ((x <component>) s) (format s "" ))

(defmethod print-object :after  ((x <component>) s) (format s ")" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.class:export-class
(defclass <composition> nil
  ((composition-components
    :accessor composition-components :initarg
    :components :documentation
    "Содержит список компонентов. 
Элементами этого списка д.б. данные типа <component>"))
  (:documentation
   "Представляет смесь, состоящую из объектов класса <component>."))

(defmethod print-object :before ((x <composition>) s)
  (format s
	  "#<composition>(composition-components=~S"
	  (composition-components x)))

(defmethod print-object ((x <composition>) s)
  (format s "" ))

(defmethod print-object :after ((x <composition>) s)
  (format s ")" ))
