;;;; classes.lisp

(defpackage #:gases/molecule
  (:use #:cl)
  (:nicknames "G-M")
  (:export <molecule> 
           <molecule>-name-en-short
           <molecule>-formula           
           <molecule>-μcp-a-b-c
           <molecule>-name-ru 
           <molecule>-mass
           <molecule>-note
           <molecule>-smile
           <molecule>-name-en)
  (:documentation "Позволяет вычислять:
1) Для отдельных компонентов:
 - молекулярную массу компонента - μ;
 - Мольную (массовую) изобарную теплоёмкость - μCp (Cp);
 - Мольную (массовую) изохорную теплоёмкость - μCv (Cv);
 - Коэффициент адиабаты - k.
2) Для смеси компонетов, заданной мольными долями:
 - кажущуюся молекулярную массу - μ-mixture;
 - массовую изобарную теплоёмкость - Cp-mixture;
 - массовую изохорную теплоёмкость - Cv-mixture;
 - Коэффициент адиабаты - k-mixture.

Данные взяты из файла ./doc/111.jpg (см. мультитехнический справочник)."))

(in-package :gases/molecule)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <molecule> ()
  ((name-ru
    :accessor <molecule>-name-ru :initarg :name-ru
    :initform ""
    :documentation
    "Обозначение русскоязычное")
   (name-en
    :accessor <molecule>-name-en
    :initarg :name-en
    :initform "" :documentation
    "Обозначение англоязычное")
   (name-en-short
    :accessor <molecule>-name-en-short :initarg
    :name-en-short :initform ""
    :documentation
    "Короткое англоязычное обозначение")
   (smile
    :accessor <molecule>-smile :initarg :smile :initform "" :documentation "Smile")
   (mass
    :accessor <molecule>-mass :initarg :mass :initform ""
    :documentation "Молекулярная масса кг/моль")
   (μcp-a-b-c
    :accessor <molecule>-μcp-a-b-c :initarg :μcp-a-b-c
    :initform ""
    :documentation
    "Коэффициенты для расчета мольной теплоемкости ккал/(моль*К). 
Данные взяты из файла ./doc/111.jpg (см. мультитехнический справочник Интернет).")
   (formula
    :accessor <molecule>-formula :initarg :formula
    :initform "" :documentation "Химическая формула")
   (note :accessor <molecule>-note :initarg :note :initform ""
                  :documentation "Примечание"))
  (:documentation "Представляет молекулу вещества."))

(defmethod print-object :before ((x <molecule>) s)
  (format s " #<molecule>(~S ~S ~S ~S ~S ~S"
	  (<molecule>-name-en-short x)
	  (<molecule>-name-ru x)
	  (<molecule>-μcp-a-b-c x)
	  (<molecule>-mass x)
	  (<molecule>-formula x)
	  (<molecule>-note x)))

(defmethod print-object         ((x <molecule>) s) (format s "" ))

(defmethod print-object :after  ((x <molecule>) s) (format s ")" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; "Данные взяты из файла ./doc/111.jpg (см. мультитехнический справочник)."
(progn
(export '*Air*) (defparameter *Air*          (make-instance '<molecule> :name-en-short "Air"          :μcp-a-b-c '( 6.557    1.477   -0.2148 ) :mass 0.02896      :name-ru "Воздух"              :formula "Воздух"     :note "0.02896"))
(export '*NH3*) (defparameter *NH3*          (make-instance '<molecule> :name-en-short "NH3"          :μcp-a-b-c '( 6.086    8.812   -1.506  ) :mass 0.0170306    :name-ru "Аммиак"              :formula "NH3"        :note "(+ 14 3)"))
(export '*Br2*) (defparameter *Br2*          (make-instance '<molecule> :name-en-short "Br2"          :μcp-a-b-c '( 8.4228   0.9739  -0.3555 ) :mass 0.159808     :name-ru "Бром"                :formula "Br2"        :note "(* 79.904 2)"))
(export '*CO2*) (defparameter *CO2*          (make-instance '<molecule> :name-en-short "CO2"          :μcp-a-b-c '( 6.214   10.396   -3.545  ) :mass 0.04401      :name-ru "Двуокись углерода"   :formula "CO2"        :note "(+ (* 12 1) (* 16 2))"))
(export '*CO*)  (defparameter *CO*           (make-instance '<molecule> :name-en-short "CO"           :μcp-a-b-c '( 6.420    1.665   -0.196  ) :mass 0.02801      :name-ru "Окись углерода"      :formula "CO"         :note "(+ (* 12 1) (* 16 1))"))
(export '*Cl2*) (defparameter *Cl2*          (make-instance '<molecule> :name-en-short "Cl2"          :μcp-a-b-c '( 7.5755   2.4244  -0.9650 ) :mass 0.070903     :name-ru "Хлор"                :formula "Cl2"        :note "(+ 35.446 35.457)"))
(export '*H2*)  (defparameter *H2*           (make-instance '<molecule> :name-en-short "H2"           :μcp-a-b-c '( 6.9469  -0.1999   0.4808 ) :mass 0.0020159502 :name-ru "Водород"             :formula "H2"         :note "(+ 1.00784 1.00811)"))
(export '*HBr*) (defparameter *HBr*          (make-instance '<molecule> :name-en-short "HBr"          :μcp-a-b-c '( 5.5776   0.9549   0.1581 ) :mass 0.08091      :name-ru "Бромистый водород"   :formula "HBr"        :note "80.91"))
(export '*HCl*) (defparameter *HCl*          (make-instance '<molecule> :name-en-short "HCl"          :μcp-a-b-c '( 6.732    0.4325   0.3697 ) :mass 0.0364606    :name-ru "Хлористый водород"   :formula "HCl"        :note "36.46061"))
(export '*H2S*) (defparameter *H2S*          (make-instance '<molecule> :name-en-short "H2S"          :μcp-a-b-c '( 6.662    5.134   -0.854  ) :mass 0.034082     :name-ru "Сероводород"         :formula "H2S"        :note "34.082"))
(export '*N2*)  (defparameter *N2*           (make-instance '<molecule> :name-en-short "N2"           :μcp-a-b-c '( 6.4492   1.4125  -0.0807 ) :mass 0.02801371   :name-ru "Азот"                :formula "N2"         :note "(+ 14.00643 14.00728)"))
(export '*NO*)  (defparameter *NO*           (make-instance '<molecule> :name-en-short "NO"           :μcp-a-b-c '( 6.440    2.069   -0.4206 ) :mass 0.0300061    :name-ru "Оксид азота"         :formula "NO"         :note "30.0061"))
(export '*N2O*) (defparameter *N2O*          (make-instance '<molecule> :name-en-short "N2O"          :μcp-a-b-c '( 6.529   10.515   -3.571  ) :mass 0.0440128    :name-ru "Закись азота"        :formula "N2O"        :note "44.0128"))
(export '*O2*)  (defparameter *O2*           (make-instance '<molecule> :name-en-short "O2"           :μcp-a-b-c '( 6.0954   3.2533  -1.0171 ) :mass 0.0319988    :name-ru "Кислород"            :formula "O2"         :note "O2 (+ 15.99903 15.99977)"))
(export '*PH3*) (defparameter *PH3*          (make-instance '<molecule> :name-en-short "PH3"          :μcp-a-b-c '( 4.496   14.372   -4.072  ) :mass 0.03400      :name-ru "Фосфин"              :formula "PH3"        :note "34.00"))
(export '*SO2*) (defparameter *SO2*          (make-instance '<molecule> :name-en-short "SO2"          :μcp-a-b-c '( 7.116    9.512   -3.511  ) :mass 0.0640540    :name-ru "Двуокись серы"       :formula "SO2"        :note "64.054"))
(export '*SO3*) (defparameter *SO3*          (make-instance '<molecule> :name-en-short "SO3"          :μcp-a-b-c '( 6.077   23.537   -9.687  ) :mass 0.08006      :name-ru "Серный ангидрид"     :formula "SO3"        :note "80.06"))
(export '*H2O*) (defparameter *H2O*          (make-instance '<molecule> :name-en-short "H2O"          :μcp-a-b-c '( 7.256    2.298    0.283  ) :mass 0.01801528   :name-ru "Вода"                :formula "H2O"        :note "!!! (+ (* 16 1) 2)"))

(export '*C1*)  (defparameter *C1*           (make-instance '<molecule> :name-en-short "C1"           :μcp-a-b-c '( 3.381   18.044   -4.300 ) :mass 0.01604      :name-ru "Метан"               :formula "CH4"        :note "!!! (+ (* 12 1) 4)" ))
(export '*C2*)  (defparameter *C2*           (make-instance '<molecule> :name-en-short "C2"           :μcp-a-b-c '( 2.247   38.201  -11.049 ) :mass 0.03007      :name-ru "Этан"                :formula "C2H6"       :note "(+ (* 12 2) 6)"))
(export '*C3*)  (defparameter *C3*           (make-instance '<molecule> :name-en-short "C3"           :μcp-a-b-c '( 2.410   57.195  -17.533 ) :mass 0.04410      :name-ru "Пропан"              :formula "C3H8"       :note "(+ (* 12 3) 8)"))
(export '*nC4*) (defparameter *nC4*          (make-instance '<molecule> :name-en-short "nC4"          :μcp-a-b-c '( 4.453   72.270  -22.214 ) :mass 0.05812      :name-ru "н-Бутан"             :formula "C4H10"      :note "(+ (* 12 4) 10)"))
(export '*iC4*) (defparameter *iC4*          (make-instance '<molecule> :name-en-short "iC4"          :μcp-a-b-c '( 3.332   75.214  -23.734 ) :mass 0.05812      :name-ru "2-Метил-Пропан"      :formula "C4H10"      :note "изо-Бутан (+ (* 12 4) 10)"))

(export '*nC5*) (defparameter *nC5*          (make-instance '<molecule> :name-en-short "nC5"          :μcp-a-b-c '( 5.910   88.449  -27.388 ) :mass 0.07215      :name-ru "н-Пентан"            :formula "C5H12"      :note "(+ (* 12 5) 12)"))
(export '*iC5*) (defparameter *iC5*          (make-instance '<molecule> :name-en-short "iC5"          :μcp-a-b-c '( 5.910   88.449  -27.388 ) :mass 0.07215      :name-ru "изо-Пентан"          :formula "C5H12"      :note "molecule-μcp-a-b-c приняты по н-Пентану (+ (* 12 5) 12)")) 

(export '*nC6*)          (defparameter *nC6*          (make-instance '<molecule> :name-en-short "C6"           :μcp-a-b-c '( 7.477  104.422  -32.471 ) :mass 0.08617848   :name-ru "н-Гексан"            :formula "C6H14"      :note "(+ (* 12 6) 14)"))
(export '*nC7*)          (defparameter *nC7*          (make-instance '<molecule> :name-en-short "C7"           :μcp-a-b-c '( 9.055  120.352  -37.528 ) :mass 0.10021      :name-ru "н-Гептан"            :formula "C7H16"      :note "(+ (* 12 7) 16)"))
(export '*nC8*)          (defparameter *nC8*          (make-instance '<molecule> :name-en-short "C8"           :μcp-a-b-c '(10.626  136.298  -42.592 ) :mass 0.11423      :name-ru "н-Октан"             :formula "C8H18"      :note "(+ (* 12 8) 18)"))
(export '*C2H4*)         (defparameter *C2H4*         (make-instance '<molecule> :name-en-short "C2H4"         :μcp-a-b-c '( 2.830   28.601   -8.726 ) :mass 0.02805      :name-ru "Этилен"              :formula "C2H4"       :note "28.05"))
(export '*C3H6*)         (defparameter *C3H6*         (make-instance '<molecule> :name-en-short "C3H6"         :μcp-a-b-c '( 3.253   45.116  -13.740 ) :mass 0.04208      :name-ru "Пропилен"            :formula "C3H6"       :note "42.08"))
(export '*1-C4H8*)       (defparameter *1-C4H8*       (make-instance '<molecule> :name-en-short "1-C4H8"       :μcp-a-b-c '( 5.132   61.760  -19.322 ) :mass 0.05611      :name-ru "1-Бутен"             :formula "C4H8"       :note "56.11"))
(export '*cys-2-C4H8*)   (defparameter *cys-2-C4H8*   (make-instance '<molecule> :name-en-short "cys-2-C4H8"   :μcp-a-b-c '( 1.625   64.836  -20.047 ) :mass 0.05611      :name-ru "цис-2-Бутен"         :formula "C4H8"       :note "56.11"))
(export '*trans-2-C4H8*) (defparameter *trans-2-C4H8* (make-instance '<molecule> :name-en-short "trans-2-C4H8" :μcp-a-b-c '( 4.967   59.961  -18.147 ) :mass 0.05611      :name-ru "транс-2-Бутен"       :formula "C4H8"       :note "56.11"))
(export '*C2H2*)         (defparameter *C2H2*         (make-instance '<molecule> :name-en-short "C2H2"         :μcp-a-b-c '( 7.331   12.622   -3.886 ) :mass 0.026038     :name-ru "Ацетилен"            :formula "C2H4"       :note "26.038"))
(export '*C3H4*)         (defparameter *C3H4*         (make-instance '<molecule> :name-en-short "C3H4"         :μcp-a-b-c '( 6.334   30.990   -9.457 ) :mass 0.04006      :name-ru "Пропин"              :formula "C3H6"       :note "40.06"))     
(export '*2-C4H6*)       (defparameter *2-C4H6*       (make-instance '<molecule> :name-en-short "2-C4H6"       :μcp-a-b-c '( 5.700   48.207  -14.479 ) :mass 0.05409      :name-ru "2-Бутин"             :formula "C4H8"       :note "54.09"))
(export '*Бензол*)       (defparameter *Бензол*       (make-instance '<molecule> :name-en-short "Бензол"       :μcp-a-b-c '(-0.409   77.621  -26.429 ) :mass 0.07811      :name-ru "Бензол"              :formula "C6H6"       :note "78.11"))           
(export '*Толуол*)       (defparameter *Толуол*       (make-instance '<molecule> :name-en-short "Толуол"       :μcp-a-b-c '( 0.576   93.593  -31.227 ) :mass 0.09214      :name-ru "Толуол"              :formula "CH3-C6H6"   :note "92.14"))
(export '*Стирол*)       (defparameter *Стирол*       (make-instance '<molecule> :name-en-short "Стирол"       :μcp-a-b-c '( 4.074   99.731  -33.108 ) :mass 0.10415      :name-ru "Стирол"              :formula "CH2=C-C6H6" :note "104.15"))
(export '*Cyclo_C5*)     (defparameter *Cyclo_C5*     (make-instance '<molecule> :name-en-short "Cyclo_C5"     :μcp-a-b-c '(-5.763   97.377  -31.328 ) :mass 0.0701       :name-ru "Циклопентан"         :formula "C5H10"      :note "(+ (* 12 5) 10)"))
(export '*Cyclo_C6*)     (defparameter *Cyclo_C6*     (make-instance '<molecule> :name-en-short "Cyclo_C6"     :μcp-a-b-c '(-7.701  125.675  -41.584 ) :mass 0.08416      :name-ru "Циклогексан"         :formula "C6H12"      :note "84.16 (+ (* 12 6) 12)"))
(export '*Mcyclo_C5*)    (defparameter *Mcyclo_C5*    (make-instance '<molecule> :name-en-short "Mcyclo_C5"    :μcp-a-b-c '(-7.701  125.675  -41.584 ) :mass 0.08416      :name-ru "Метил-Цикло-Пентан"  :formula "CH3-C5H9"   :note "принят по Циклогексану 84.16 (+ (* 12 6) 12)"))
(export '*Mcyclo_C6*)    (defparameter *Mcyclo_C6*    (make-instance '<molecule> :name-en-short "Mcyclo_C6"    :μcp-a-b-c '(-4.624  140.877  -46.698 ) :mass 0.09819      :name-ru "Метил-Цикло-Гексан"  :formula "CH3-C6H11"  :note "98.19 (+ (* 12 7) 14)"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(export 'μ )

(defmethod μ ((mol <molecule>))
"Возвращает мольную массу компонента кг/моль

 @b(Пример использования:)
@begin[lang=lisp](code)
 (μ *air*) => 0.02896
@end(code)
"
  (<molecule>-mass mol))

(export 'μCp )

(defmethod μCp ((mol <molecule>) temp)
"Возвращает мольную изобарную теплоемкость в ккал/(моль*К)
 @b(Пример использования:)
@begin[lang=lisp](code)
 (μCp *air* 273.15) => 6.944416
@end(code)
"
  (let ((x (<molecule>-μcp-a-b-c mol)))
    (let ((a (first x))
	  (b (second x))
	  (c (third x)))
      (+ a (* b temp 0.001) (* c temp temp 0.001 0.001)))))

(export 'μCv )

(defmethod μCv ((mol <molecule>) temp)
"Возвращает мольную изохорную теплоемкость в ккал/(моль*К)
при температуре temp, [К].

 @b(Пример использования:)
@begin[lang=lisp](code)
 (μCv *air* 273.15) => 4.944416
@end(code)
"
  (- (μCp mol temp) 2))

(export 'Cp )

(defmethod Cp ((mol <molecule>) temp)
"Возвращает массовую изобарную теплоемкость в ккал/(кг*К)
при температуре temp, [К].

 @b(Пример использования:)
@begin[lang=lisp](code)
 (Cp *air* 273.15) 0.23979336
@end(code)
"
  (/ (μCp mol temp) (μ mol) 1000.0))

(export 'Cv )

(defmethod Cv ((mol <molecule>) temp)
"Возвращает массовую изохорную теплоемкость в ккал/(моль*К)
при температуре temp, [К].

 @b(Пример использования:)
@begin[lang=lisp](code)
  (Cv *air* 273.15) => 0.17073259 
@end(code)
"
  (/ (μCv mol temp) (μ mol) 1000.0))

(export 'k )

(defmethod k ((mol <molecule>) temp)
"Возвращает коэффициент адиабаты при температуре temp, [К].

 @b(Пример использования:)
@begin[lang=lisp](code)
 (k *air* 273.15)         => 1.4044967 
 (k *air* (+ 273.15 100)) => 1.3938377 
 (k *air* (+ 273.15 500)) => 1.3590313 

 (k *C1* (+ 273.15 0))    => 1.3339516 
 (k *C1* (+ 273.15 100))  => 1.2661209 
 (k *C1* (+ 273.15 500))  => 1.1567233 
@end(code)
"
  (/  (Cp mol temp) (Cv mol temp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '*running-gas*)

(defparameter *running-gas*
  `((,*N2*       0.0003)
    (,*CO2*      0.0022)
    (,*C1*       0.7374)
    (,*C2*       0.0593)
    (,*C3*       0.1179)
    (,*iC4*      0.0131)
    (,*nC4*      0.0379)
    (,*iC5*      0.0130)
    (,*nC5*      0.0139)
    (,*nC6*      0.0017)
    (,*Cyclo_C5* 0.0004)
    (,*Cyclo_C6* 0.0003)     
    (,*nC7*      0.0001)
    (,*H2O*      0.0025)))

(export 'μ-mixture )

(defun μ-mixture (m-stuff)
"Возвращает мольную массу смеси компонент, заданной мольными долями, кг/моль.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (μ-mixture *running-gas*) => 0.024075469
@end(code)
"
  (let ((summ-ri-μi 0))
    (mapcar
     #'(lambda (el)
	 (let ((x-i (first el))
	       (r-i (second el)))
	   (setf summ-ri-μi
		 (+ summ-ri-μi (* r-i (μ x-i))))))
     m-stuff)
    summ-ri-μi))

(export 'Cp-mixture )

(defun Cp-mixture (m-stuff temp)
"Возвращает массовую изобарную теплоемкость [ккал/(кг*К)] 
для смеси газов m-stuff при температере temp [К].

 @b(Пример использования:)
@begin[lang=lisp](code)
 (Cp-mixture *running-gas* 373) => 0.5374432
@end(code)
"
  (let ((summ-ri-μi-cpi 0)
	(summ-ri-μi 0))
    (mapcar
     #'(lambda (el)
	 (let
	     ((x-i (first el))
	      (r-i (second el)))
	   (setf summ-ri-μi-cpi (+ summ-ri-μi-cpi (* r-i (μ x-i) (Cp x-i temp)))
		 summ-ri-μi (+ summ-ri-μi (* r-i (μ x-i))))))
     m-stuff)
    (/ summ-ri-μi-cpi summ-ri-μi)))

(export 'Cv-mixture )

(defun Cv-mixture (m-stuff temp)
"Возвращает массовую изохорную теплоемкость [ккал/(кг*К)] 
для смеси газов m-stuff при температере temp [К].

 @b(Пример использования:)
@begin[lang=lisp](code)
 (Cv-mixture *running-gas* 373) => 0.45437112 
@end(code)
"
  (let ((summ-ri-μi-cpi 0)
	(summ-ri-μi 0))
    (mapcar
     #'(lambda (el)
	 (let
	     ((x-i (first el))
	      (r-i (second el)))
	   (setf summ-ri-μi-cpi (+ summ-ri-μi-cpi (* r-i (μ x-i) (Cv x-i temp)))
		 summ-ri-μi (+ summ-ri-μi (* r-i (μ x-i))))))
     m-stuff)
    (/ summ-ri-μi-cpi summ-ri-μi)))

(export 'k-mixture )

(defun k-mixture (m-stuff temp)
"Возвращает коэффициент адиабаты для смеси газов m-stuff 
при температере temp[К].

 @b(Пример использования:)
@begin[lang=lisp](code)
 (k-mixture *running-gas* 273.15)         => 1.2330948 
 (k-mixture *running-gas* (+ 273.15 100)) => 1.1827714
@end(code)
"
  (/ (Cp-mixture m-stuff temp)
     (Cv-mixture m-stuff temp)))