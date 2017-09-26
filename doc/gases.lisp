
*N2*	  0.0003
*CO2*	  0.0022
*C1*	  0.7374
*C2*	  0.0593
*C3*	  0.1179
*iC4*	  0.0131
*nC4*	  0.0379
*iC5*	  0.0130
*nC5*	  0.0139
*nC6*	        0.0017
*Mcyclo_C5*	0.0004
*Cyclo_C6*	0.0002
*nC7*	0.0001
*Mcyclo_C6*	0.0001
*H2O*	0.0027

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *H2O*
  '(7.256 2.298  0.283  0.018)
  " Вода H2O (+ (* 16 1) 2)")

(defparameter *N2*
  '(6.4492 1.4125  -0.0807 0.014)
  " Азот N2 (* 14 2)")

(defparameter *CO2*
  '(6.214 10.396   -3.545  0.044)
  " Двуокись углерода  (+ (* 12 1) (* 16 2))")

(defparameter *C1*
  '(3.381  18.044  -4.300  0.016)
  " Метан        CH4     (+ (* 12 1) 4)")

(defparameter *C2*         '(2.247  38.201 -11.049  0.030)
  " Этан C2H6 (+ (* 12 2) 6) ")

(defparameter *C3*
  '(2.410  57.195 -17.533  0.044)
  " Пропан C3H8 (+ (* 12 3) 8)")

(defparameter *iC4*
  '(4.453  72.270 -22.214  0.058)
  " изо-Бутан принят по н-Бутану C4H10 (+ (* 12 4) 10)")

(defparameter *nC4*
  '(4.453  72.270 -22.214  0.058)
  " н-Бутан C4H10 (+ (* 12 4) 10)")

(defparameter *nC5*
  '(5.910  88.449 -27.388  0.072)
  " н-Пентан C5H12 (+ (* 12 5) 12)")

(defparameter *iC5*
  '(5.910  88.449 -27.388  0.072)
  " изо-Пентан принят по н-Пентану C5H12 (+ (* 12 5) 10)")

(defparameter *nC6*
  '(7.477 104.422 -32.471  0.086)
  " н-Гексан C6H14 (+ (* 12 6) 14)")

(defparameter *nC7*
  '(9.055 120.352 -37.528  0.100)
  " н-Гептан C7H16   (+ (* 12 7) 16)")

(defparameter *nC8*
  '(10.626 136.298 -42.592  0.114)
  " н-Октан C8H18   (+ (* 12 8) 18)")

(defparameter *Cyclo_C5*
  '(-5.763  97.377 -31.328  0.070)
  " Циклопентан  C5H10 (+ (* 12 5) 10)")

(defparameter *Cyclo_C6*
  '(-7.701 125.675 -41.584  0.084)
  " ; Циклогексан  C6H12 (+ (* 12 6) 12)")

(defparameter *Mcyclo_C5*
  '(-7.701 125.675 -41.584  0.084)
  " метил-Цеклопентан принят по Циклогексану C6H12 (+ (* 12 6) 12)")

(defparameter *Mcyclo_C6* '(9.055 120.352 -37.528   0.098)
  " метил-Цеклогексан принят по н-Гексану C7H14 (+ (* 12 7) 14)")


(defun μCp (x temp)
  "Возвращает мольную изобарную теплоемкость в ккал/(моль*К)"
  (let ((a (first x))
	(b (second x))
	(c (third x)))
    (+ a (* b temp 0.001) (* c temp temp 0.001 0.001))))

(defun μCv (x temp)
    "Возвращает мольную изохорную теплоемкость в ккал/(моль*К)"
  (- (μCp x temp) 2))

(defun Cp (x temp)
  "Возвращает массовую изобарную теплоемкость в ккал/(кг*К)"
  (let ((μ (fourth x)))
    (/ (μCp x temp) μ 1000.0)))

(defun Cv (x temp)
    "Возвращает массовую изохорную теплоемкость в ккал/(моль*К)"
  (let ((μ (fourth x)))
    (/ (μCv x temp) μ 1000.0)))

(defun k (x temp)
  (/  (Cp x temp) (Cv x temp)))



(k *H2O* 573)

(Cp *C1* 373)

(Cv *C1* 373)

(mapcar
 #'(lambda (el)
     (list el (Cp '("С1" 3.381 18.044 -4.000) (+ 273 el)))
     

     )
 '(0 100 200 300 400 500 600 700 800 900 1000))
