;;;; test.lisp

(in-package :gases)

(require :math)

(defparameter *tbl-perodic-long-1*
  (make-instance 'math:<matrix>
		 :dimensions '(10 19)
		 :initial-element t))

(loop :for i :from 0 :below (math:rows *tbl-perodic-long-1*) :do
  (loop :for j :from 0 :below (math:cols *tbl-perodic-long-1*) :do
    (setf (math:mref *tbl-perodic-long-1* i j) nil)))

(defparameter *tbl-colors*
  '((:c01 "#f66"    "Щёлочные металлы")
    (:c02 "#ffdead" "Щёлочноземельные металлы")
    (:c03 "#ffc0c0" "Переходные металлы")
    (:c04 "#ccc"    "Постпереходные металлы")
    (:c05 "#cc9"    "Полуметаллы -- металлоиды")
    (:c06 "#a0ffa0" "Другие неметаллы (16-я (VI) группа -- халькогены)")
    (:c07 "#ff9"    "Галогены")
    (:c08 "#c0ffff" "Благородные газы")
    (:c09 "#ffbfff" "Лантаноиды")
    (:c10 "#ef99cc" "Актиноиды")))

(defparameter *tbl-perodic-long*
  '(( 1  1 1 1 :M) (  2   2 1 18 :M)
    ( 3  4 2 1 :M) (  5  10 2 13 :M)
    (11 12 3 1 :M) ( 13  18 3 13 :M)
    (19 36 4 1 :M)
    (37 54 5 1 :M)
    (55 57 6 1 :M) ( 58  71 6 4 :LA) ( 72  86 6 4 :M)
    (87 89 7 1 :M) ( 90 103 7 4 :AC) (104 118 7 4 :M)))

(defun period-group-long (el-number)
  "Для элемента периодической таблицы элементов, 
заданной номером возвращает список содержащий:
1) Признак принадлежности к:
 - главной части таблицы -- :M  ;
 - группе лантаноидов    -- :LA ;
 - группе актиноидов     -- :AC .
2) период -- число от 1 до 7 ;
3) группу -- число от 1 до 18
или nil если номер элемента отсутствует в таблице.

 @b(Пример использования:)
@begin[lang=lisp](code)
(period-group-long 7) =>  (:M 2 15)
(period-group-long 13) => (:M 3 13)
@end(code)
"
  (let ((desc (find-if
	       #'(lambda (el)
		   (<= (first el) el-number (second el))
		   )
	       *tbl-perodic-long*)))
    (when desc
      (list
       (fifth desc)
       (third desc)
       (+ (fourth desc)
	  (-  el-number
	      (first desc) ))))))

(defun element-color (el-number)
  (let* ((t-p-g (period-group-long el-number))
	 (tag    (first  t-p-g))
	 (period (second t-p-g))
	 (group  (third  t-p-g))
	 )
    (cond
      ((and (eq :LA tag)) (assoc :c09 *tbl-colors* ))
      ((and (eq :AC tag)) (assoc :c10 *tbl-colors* ))
      ((and (eq :M  tag) (= group 1) (<= 2 period 7) ) (assoc :c01 *tbl-colors* ))
      ((and (eq :M  tag) (= group 2) (<= 2 period 7) ) (assoc :c02 *tbl-colors* ))
      ((and (eq :M  tag) (<= 3 group 12) (<= 4 period 7) ) (assoc :c03 *tbl-colors* ))
      ((and (eq :M  tag)
	    (or
	     (and (= group 13) (<= 3 period 7))
	     (and (= group 14) (<= 5 period 7))
	     (and (= group 15) (<= 6 period 7))
	     (and (= group 16) (=    period 7))))
       (assoc :c04 *tbl-colors* ))
      ((and (eq :M  tag)
	    (or
	     (and (= group 13) (=    period 2))
	     (and (= group 14) (<= 3 period 4))
	     (and (= group 15) (<= 4 period 5))
	     (and (= group 16) (<= 5 period 6))))
       (assoc :c05 *tbl-colors* ))
      ((and (eq :M  tag)
	    (or
	     (and (= period 1) (=    group 1) )
	     (and (= period 2) (<= 14 group 16))
	     (and (= period 3) (<= 15 group 16))
	     (and (= period 4) (=     group 16))))
       (assoc :c06 *tbl-colors* ))
      ((and (eq :M  tag) (= group 17) (<= 2 period 7) )
       (assoc :c07 *tbl-colors* ))
      ((and (eq :M  tag) (= group 18) (<= 1 period 7) )
       (assoc :c08 *tbl-colors* ))
      (t t-p-g))))

(element-color 2)

(period-group-long 90)


(require :periodic-table)

(elements:element-name (elements:atomic-number-element 12))

(type-of (elements:atomic-number-element 12)) ; => ELEMENTS:ELEMENT


(defmethod html-out ((el t) s)
  (when el (format s "~A" el)))

(defmethod html-out ((el ELEMENTS:ELEMENT) s)
  "Новая версия 01"
  (format s 
	  (cl-who:with-html-output-to-string (o-str nil :indent t)
	    (cl-who:htm
	     (:form :action "mg-edit"	      
	      (:table :style
		      (cl-who:conc "background-color:" (second (element-color (elements:element-atomic-number el))) "; "
				   "color:black; width:6em") ;;;; 
		      (:tr (:td (:div :style "font-size:50%" (cl-who:fmt "~A" (elements:element-atomic-number el)))
				(:input :name (cl-who:conc "elements" "[" (elements:element-symbol el) "]" "[ch-box]") :type "checkbox" )
				(cl-who:fmt "~A" (elements:element-symbol el))
				(:input :name (cl-who:conc "elements" "[" (elements:element-symbol el) "]" "[l-e-b]") :list "less-eq-big"
					:style "font-size:65%; width:2.5em;")
				(:datalist :id "less-eq-big"
					   (:option :value "<")
					   (:option :value "=")
					   (:option :value ">"))))
		      (:tr (:td (:input :name (cl-who:conc "elements" "[" (elements:element-symbol el) "]" "[num]")
					:type "number"
					:style "width:6em"
					:min "1"
					:max "100"))))
	      (:input :type "submit"))))))

(defmethod html-out ((el ELEMENTS:ELEMENT) s)
  "Новая версия 02"
  (format s 
	  (cl-who:with-html-output-to-string (o-str nil :indent t)
	    (cl-who:htm
	     (:table :style
		      (cl-who:conc "background-color:" (second (element-color (elements:element-atomic-number el))) "; "
				   "color:black; width:6em") ;;;; 
		      (:tr (:td (:div :style "font-size:50%" (cl-who:fmt "~A" (elements:element-atomic-number el)))
				(:input :name (cl-who:conc "elements" "[" (elements:element-symbol el) "]" "[ch-box]") :type "checkbox" )
				(cl-who:fmt "~A" (elements:element-symbol el))
				(:input :name (cl-who:conc "elements" "[" (elements:element-symbol el) "]" "[l-e-b]") :list "less-eq-big"
					:style "font-size:65%; width:2.5em;")
				(:datalist :id "less-eq-big"
					   (:option :value "<")
					   (:option :value "=")
					   (:option :value ">"))))
		      (:tr (:td (:input :name (cl-who:conc "elements" "[" (elements:element-symbol el) "]" "[num]")
					:type "number"
					:style "width:6em"
					:min "1"
					:max "100"))))
))))

;;;; (let ((o-str (make-string-output-stream))) (get-output-stream-string o-str)))

(html-out (elements:atomic-number-element 12) t)

(defmethod html-out ((mm math:<matrix>) s)
  "Старая версия"
  (format s "~%<table>")
  (loop :for i :from 0 :below (math:rows mm) :do
    (let ((rr (math:row mm i)))
      (format s "~%<tr>")
      (map nil
	   #'(lambda (el)
	       (format s "<td>")
	       (html-out el s)
	       (format s "</td>"))
	   rr)
      (format s "~%</tr>")))
  (format s "~%</table>"))

(defmethod html-out ((mm math:<matrix>) s)
  "Новая версия"
  (let ((o-str (make-string-output-stream)))
    (format o-str "~%<table>")
    (loop :for i :from 0 :below (math:rows mm) :do
      (let ((rr (math:row mm i)))
	(format o-str "~%<tr>")
	(map nil
	     #'(lambda (el)
		 (format o-str "<td>")
		 (html-out el o-str)
		 (format o-str "</td>"))
	     rr)
	(format o-str "~%</tr>")))
    (format o-str "~%</table>")
    (format s "~A" (get-output-stream-string o-str))))

(loop :for i :from 1 :to 112 :do
  (let ((t-p-g (period-group-long i)))
    (when (and t-p-g (eq :M (first t-p-g)))
      (setf (math:mref *tbl-perodic-long-1*
		       (second t-p-g) 
		       (third  t-p-g))
	    (elements:atomic-number-element i)))
    (when (and t-p-g (eq :LA (first t-p-g)))
      (setf (math:mref *tbl-perodic-long-1*
		       (+ (second t-p-g) 2)
		       (third  t-p-g))
	    (elements:atomic-number-element i)))
    (when (and t-p-g (eq :AC (first t-p-g)))
      (setf (math:mref *tbl-perodic-long-1*
		       (+ (second t-p-g) 2)
		       (third  t-p-g))
	    (elements:atomic-number-element i)))))

(html-out *tbl-perodic-long-1* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(relativ-air-mass-for-burning (make-instance-composition '(("CH4" 0.9) ("H2" 0.1))))

(molar-mass (make-instance-composition '(("CH4" 1.0) ("H2" 0.0))))

(density (make-instance-component "CH4" 1.0) 101325.0 273.)
(density (make-instance-composition '(("CH4" 0.8) ("H2" 0.2))) 101325.0 273.)
(density (make-instance-composition '(("CH4" 0.9) ("H2" 0.1))) 101325.0 273.)
(density (make-instance-composition '(("CH4" 0.95) ("H2" 0.05))) 101325.0 273.)


(density (get-sp "H2") *p-normal*   *t-normal*)
(density (get-sp "CH4") *p-normal*  293.15)

(combustion-reaction (get-sp "C2H4"))



(Q-work-low (get-sp "H2"))
(Q-work-low (make-instance-component "H2" 0.030457929595689225d0  :mass))
(Q-work-low (make-instance-composition '(("H2" 0.2) ("CH4" 0.80))))

(wobber-low (make-instance-composition '(("H2" 0.2) ("CH4" 0.80))))

(density (make-instance-composition '(("H2" 0.2) ("CH4" 0.80))) 101325.0 273.15 )
'(("H2" 0.2) ("CH4" 0.80))

(molar-mass (make-instance-composition '(("H2" 0.2) ("CH4" 0.80))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *c-n* (make-instance-composition '(("H2" 0.95) ("CH4" 0.05))))


(relativ-air-mass-for-burning *c-n*)
(thermal-effect )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



@annot.doc:doc
"
"
(defun select (&key atoms designation description)
  (when atoms
    (maphash
     #'(lambda (key value)
	 )
     (get-db))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(get-sp "NaOH")
(get-sp "H2O")
(get-sp	"NaCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (remove-method #'ADAPT-MOLE-FRACTIONS (find-method #'ADAPT-MOLE-FRACTIONS '() (mapcar #'find-class '(t t))))

(with-open-file (fl "/home/namatv/quicklisp/local-projects/clisp/gases/data/termo.inp"
		    :direction :output :if-exists :supersede)
  (dump (get-db) fl))

(relativ-air-mass-for-burning (make-instance-composition '(("CH4" 0.1) ("C6H6" 0.9)) :mass))


(defparameter *sp* (get-sp "CH4"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-sp "Air")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *rt*  
  (make-instance '<reactant> :species (get-sp "H2O") :mole 2))

(molar-mass *rt*)

(thermal-effect *rt*)

(sp-heat-formation (species *rt*))



(defparameter *reac*
  (make-instance '<reaction> :reactant-names '("C2H5OH" "O2") :product-names '("H2O" "CO2")))

(defparameter *reac*
  (make-instance '<reaction> :reactant-names '("C2H5OH" "O2") :product-names '("H2O(L)" "CO2")))

(defparameter *reac*
  (make-instance '<reaction> :reactant-names '("H2" "O2") :product-names '("H2O")))

(defparameter *reac*
  (make-instance '<reaction> :reactant-names '("CH4" "O2") :product-names '("H2O(L)" "CO2")))

(thermal-effect *reac*)

(culc-koeffitients *reac*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((sp (get-sp (first '("H2O" "CH4" "N2" "O2" "Air")) ))
      (tt 2500.d0))
  (list (molar-isobaric-heat-capacity sp tt)
	(molar-isochoric-heat-capacity sp tt)
	(molar-enthalpy sp tt)
	(molar-entropy  sp tt)
	(adiabatic-index sp tt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(apply #'+
       (mapcar #'(lambda (el)
		   (let ((elm (get-sp (first el) )))
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

(apply #'+
       (mapcar #'(lambda (el)
		   (let ((elm (get-sp (first el) )))
		     (* (sp-molar-mass elm ) (second el))))
	       '(
		 ("CO2"	                0.0739)
		 ("N2"	                0.0004)
		 ("CH4"	                0.8134 "C1")
		 ("C2H6"	        0.0558)
		 ("C3H8"	        0.0367)
		 ("C4H10,isobutane" 	0.0061)
		 ("C4H10,n-butane"	0.0087)
		 ("C5H12,i-pentane" 	0.0023)
		 ("C5H12,n-pentane"	0.0016)
		 ("C6H14,n-hexane" 	0.0006)
;;;;		 ("C6H12,1-hexene"      0.0004 "Mcyclo_C5")
;;;;		 ("C6H12,cyclo-"	0.0002)
		 ("C7H16,n-heptane"     0.0005)
;;;;		 ("C7H14,1-heptene" 	0.0001 "Mcyclo_C6")
;;;;		 ("H2O"	                0.0025)
		 )))

(get-sp "N2" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod molar-mass ((x <molecule>))
  "Возвращает молекулярную массу, [g/mol]
Пример использования
(molar-mass (get-sp \"N2\" )) => 28.0134
(molar-mass (get-sp \"CH4\" )) => 16.04246
"
  (molecule-mass x))

(molar-mass *air*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(let ((test (make-instance
	      'composition :components
	      (list (make-instance 'component :species (get-sp "N2"              ) :mole-fraction 0.0003 )
		    (make-instance 'component :species (get-sp "CO2"             ) :mole-fraction 0.0022 )
		    (make-instance 'component :species (get-sp "CH4"             ) :mole-fraction 0.7374 )
		    (make-instance 'component :species (get-sp "C2H6"            ) :mole-fraction 0.0593 )
		    (make-instance 'component :species (get-sp "C3H8"            ) :mole-fraction 0.1179 )
		    (make-instance 'component :species (get-sp "C4H10,isobutane" ) :mole-fraction 0.0131 )
		    (make-instance 'component :species (get-sp "C4H10,n-butane"  ) :mole-fraction 0.0379 )
		    (make-instance 'component :species (get-sp "C5H12,i-pentane" ) :mole-fraction 0.0130 )
		    (make-instance 'component :species (get-sp "C5H12,n-pentane" ) :mole-fraction 0.0139 )
		    (make-instance 'component :species (get-sp "C6H14,n-hexane"  ) :mole-fraction 0.0017 )
		    (make-instance 'component :species (get-sp "C6H10,cyclo-"    ) :mole-fraction 0.0004 )
		    (make-instance 'component :species (get-sp "C6H10,cyclo-"    ) :mole-fraction 0.0002 )
		    (make-instance 'component :species (get-sp "C7H16,n-heptane" ) :mole-fraction 0.0001 )
		    (make-instance 'component :species (get-sp "C6H10,cyclo-"    ) :mole-fraction 0.0001 )
		    (make-instance 'component :species (get-sp "H2O"             ) :mole-fraction 0.0027 )))))
  (adiabatic-index test 473))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;T 220      230       240     250      260      270       280      290
;P 2.2      2.2       2.2     2.2      2.2      2.2       2.2      2.2
;H -4818.52 -4797.44 -4776.26 -4754.96 -4733.53 -4711.94 -4690.18 -4668.22
;U -4932.54 -4916.64 -4900.64 -4884.53 -4868.28 -4851.88 -4835.30 -4818.52
;G -7140.57 -7246.59 -7353.53 -7461.36 -7570.03 -7679.54 -7789.85 -7900.94
;S  10.5548  10.6885  10.7387  10.8256  10.9096  10.9911  11.0703  11.1473


;T 220      230       240     250      260      270       280      290
;P 26.      26.       26.     26.      26.      26.       26.      26.
;H -4818.52 -4797.44 -4776.26 -4754.96 -4733.53 -4711.94 -4690.18 -4668.22
;U -4932.54 -4916.64 -4900.64 -4884.53 -4868.28 -4851.88 -4835.30 -4818.52
;G -6858.98 -6952.20 -7046.34 -7141.36 -7237.24 -7333.95 -7431.46 -7529.75
;S  9.27488   9.3685   9.4587   9.5456   9.6297   9.7111   9.7903   9.8673





