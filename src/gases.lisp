;;;; gases.lisp

(in-package :gases)

(export '*Rμ*)
(defvar *Rμ* 8.31446261815324d0
  "Унинверсальная газовая постоянная [м2*кг*с-2*К-1*Моль-1]")

(export '*kal*)
(defvar *kal* 4.1868
  "Численное значение международной калории в Джоулях, Дж (J)")

(export '*C-0*)
(defvar *C-0* 273.15
  "Ноль шкалы Цельсия в Кельвинах, К (K)")

(export '*t-normal*)
(defvar *t-normal* 273.15
  "Нормальная температура, К (K)")

(export '*t-standard*)
(defvar *t-standard* 298.15
  "Стандартная температура, К (K)")


(export '*P-normal*)
(defvar *P-normal* 101325.0d0
  "Нормальное атмоферное давление в Паскалях, Па")

(export '*P-standard*)
(defvar *P-standard* 100000.0d0
    "Стандартное давление в Паскалях, Па")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

    


