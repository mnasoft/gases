;;;; gases.lisp

(in-package :gases)

(annot:enable-annot-syntax)

@export
(defvar *Rμ* 8.3144598d0
  "Унинверсальная газовая постоянная [м2*кг*с-2*К-1*Моль-1]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defun μ (x)
  "Возвращает мольную массу компонента кг/моль"
  (fourth x))

@export
(defun μCp (x temp)
  "Возвращает мольную изобарную теплоемкость в ккал/(моль*К)"
  (let ((a (first x))
	(b (second x))
	(c (third x)))
    (+ a (* b temp 0.001) (* c temp temp 0.001 0.001))))

@export
(defun μCv (x temp)
    "Возвращает мольную изохорную теплоемкость в ккал/(моль*К)"
  (- (μCp x temp) 2))

@export
(defun Cp (x temp)
  "Возвращает массовую изобарную теплоемкость в ккал/(кг*К)"
  (let ((μ (μ x)))
    (/ (μCp x temp) μ 1000.0)))

@export
(defun Cv (x temp)
    "Возвращает массовую изохорную теплоемкость в ккал/(моль*К)"
  (let ((μ (μ x)))
    (/ (μCv x temp) μ 1000.0)))

@export
(defun k (x temp)
  (/  (Cp x temp) (Cv x temp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defun μ-mixture (m-stuff)
  "Возвращает мольную массу компонента кг/моль"
  (let ((summ-ri-μi 0))
    (mapcar
     #'(lambda (el)
	 (let
	     ((x-i (first el))
	      (r-i (second el)))
	   (setf summ-ri-μi (+ summ-ri-μi (* r-i (μ x-i))))))
     m-stuff)
     summ-ri-μi))

@export
(defun Cp-mixture (m-stuff temp)
  "Возвращает массовую изобарную теплоемкость [ккал/(кг*К)] для смеси газов m-stuff при температере temp[К].
Пример использования:
(Cp-mixture *running-gas* 373) => 0.5388392"
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

@export
(defun Cv-mixture (m-stuff temp)
    "Возвращает массовую изохорную теплоемкость [ккал/(кг*К)] для смеси газов m-stuff при температере temp[К].
Пример использования:
(Cv-mixture *running-gas* 373) => 0.45559332"
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

@export
(defun k-mixture (m-stuff temp)
  "Возвращает коэффициент адиабаты для смеси газов m-stuff при температере temp[К]."
  (/ (Cp-mixture m-stuff temp)
     (Cv-mixture m-stuff temp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
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

@export
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

@export
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

@export
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

@export
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

    


