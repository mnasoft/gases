;;;; gasodinamic-func.lisp

(in-package #:cl-user)

(defpackage #:gases.gasodinamic
  (:use #:cl)
  (:nicknames "gasodinamic" "gd")
  )

(in-package #:gases.gasodinamic)

;;;;
;;;; Число Маха - отношение скорости потока к местной скорости звука.
;;;; Приведенная скорость (коэффициент скорости) - отношение скорости потока к критической скорости.

(export 'Mah-by-lambda )

(defun Mah-by-lambda (lam k)
"@b(Описание:) возвращает значение числа Маха по значению приведенной скорости lam и коэффициента адиабаты k.
@b(Переменые:)
@begin(list)
 @item(lam - приведенная скорость (коэффициент скорости) - отношение скорости потока к критической скорости;)
 @item(k  - коэффициента адиабаты - отношение отношение изобарной и изохорной теплоемкостей Cp/Cv.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (Mah-by-lambda 1.8212 1.4) 
@end(code)
"
  (/
   (* lam (sqrt (/ 2 (+ k 1))))
   (sqrt (- 1 (* (/ (- k 1) (+ k 1)) (expt lam 2))))))

(export 'Lambda-by-mah )

(defun Lambda-by-mah (Mah k)
"@b(Описание:) lambda-by-mah - газодинамическая функция идеального газа 
возвращает значение приведенной скорости lam по значению числа Маха и коэффициента адиабаты k.

 @b(Переменые:)
@begin(list) 
 @item(Mah - число Маха - отношение скорости потока к местной скорости звука V/A;) 
 @item(k   - коэффициента адиабаты - отношение отношение изобарной и изохорной теплоемкостей Cp/Cv.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (Lambda-by-mah 51.5 1.4) 
@end(code)
"
  (/ (* Mah (sqrt (/ (+ k 1) 2)))
     (sqrt (+ 1 (* (/ (- k 1) 2) (expt Mah 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'tau-by-lambda )

(defun tau-by-lambda (lam k)
"@b(Описание:) Газодинамическая функция идеального газа
возвращает отношение температуры в данном сечении к температуре заторможенного потока T/T0.

 @b(Переменые:)
@begin(list)
 @item(lam - приведенная скорость (коэффициент скорости) - отношение скорости потока к скорости звука в критическом сечении V/Aкр;)
 @item(k   - коэффициента адиабаты - отношение отношение изобарной и изохорной теплоемкостей Cp/Cv.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (tau-lambda 1. 1.4)
@end(code)
"
  (- 1 (* (/ (- k 1) (+ k 1)) (expt lam 2))))

(export '1/tau-by-mah )

(defun 1/tau-by-mah (mah k)
"@b(Описание:) 1/tau-by-mah - газодинамическая функция идеального газа
возвращает отношение температуры в данном сечении к температуре заторможенного потока T0/T.

 @b(Переменые:)
@begin(list)
 @item(lam - приведенная скорость (коэффициент скорости) - отношение скорости потока к скорости звука в критическом сечении V/Aкр;)
 @item(k   - коэффициента адиабаты - отношение отношение изобарной и изохорной теплоемкостей Cp/Cv.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let* ((l 1.5)
       (m(mah-by-lambda  l 1.4)))
 (values l  m (tau-lambda l 1.4) (/ (1/tau-by-mah m 1.4))))
 
@end(code)
"
  (+ 1 (* (- k 1) 1/2 (expt mah 2))))

(export 'pi-by-lambda )

(defun pi-by-lambda (Lam k)
"@b(Описание:) pi-by-lambda - газодинамическая функция идеального газа
возвращает отношение давления в данном сечении к давлению заторможенного потока P/P0.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (pi-by-lambda 1.8212 1.4) => 0.059809975
@end(code)
"
  (expt (- 1 (* (/ (- k 1) (+ k 1)) (expt Lam 2))) (/ k (- k 1))))

(export 'epsilon-by-lambda )

(defun epsilon-by-lambda (Lam k)
"@b(Описание:) epsilon-by-lambda - газодинамическая функция идеального газа
возвращает отношение плотности в данном сечении к плотности заторможенного потока ρ/ρ0.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (epsilon-by-lambda 1.8212 1.4) => 0.1337417
@end(code)
"
  (expt (- 1 (* (/ (- k 1) (+ k 1)) (expt Lam 2))) (/ 1 (- k 1))))

(export 'q-by-lambda )

(defun q-by-lambda (lam k)
"@b(Описание:) q-by-lambda - газодинамическая функция идеального газа
возвращает отношение массового потока в данном сечении к массовому потоку в критическом сечении
безразмерная плотность тока (ρ*w)/(ρ*w)кр.
@begin[lang=lisp](code)
 (q-by-lambda 0.0000 1.4) => 0.000000
 (q-by-lambda 0.2500 1.4) => 0.384170
 (q-by-lambda 0.5000 1.4) => 0.709111
 (q-by-lambda 0.7500 1.4) => 0.924984

 (q-by-lambda 1.0000 1.4) => 1.000000
 (q-by-lambda 1.3623 1.4) => 0.851988
 (q-by-lambda 1.6293 1.4) => 0.596609
 (q-by-lambda 1.8212 1.4) => 0.384218
@end(code)
"
  (* lam
     (expt
      (* (/ (+ k 1) 2)
	 (- 1 (* (/ (- k 1) (+ k 1)) (expt lam 2))))
      (/ 1 (- k 1)))))

(export 'lambda-upper-bound )

(defun lambda-upper-bound (k)
"Определяет верхнюю границу для диапазода значений lambda."
  (sqrt (/ (+ k 1) (- k 1))))

(export 'lambda-by-q )

(defun lambda-by-q (q k)
"lambda-by-q - функция обранная q-by-lambda при постоянном значении k.
Функция возвращает два значения:
@begin(list)
 @item(первое для докритической области;)
 @item(второе для закритической области.)
@end(list)
"
  (labels
      ((closure-lambda-by-q (q k)
       #'(lambda (lam)
	   (- (q-by-lambda lam k) q))))
    (values
     (half-div:h-div 0.0 1.0
		     (closure-lambda-by-q q k))
     (half-div:h-div 1.0 (lambda-upper-bound k)
		     (closure-lambda-by-q q k)))))

(export 'lambda-by-tau )

(defun lambda-by-tau (tau k)
"@begin[lang=lisp](code)
 (lambda-by-tau 0.9583333 1.4)
 (tau-by-lambda 0.5 1.4) 0.9583333
@end(code)
"
  (labels
      ((closure-lambda-by-tau (tau k)
	 #'(lambda (lam)
	     (- (tau-by-lambda lam k) tau))))
    (half-div:h-div 0.0 (lambda-upper-bound k)
		    (closure-lambda-by-tau tau k))))

(export 'lambda-by-pi )

(defun lambda-by-pi (pii k)
"@b(Пример использования:)
@begin[lang=lisp](code)
 (pi-by-lambda 0.9 1.4)         => 0.6019444
 (pi-by-lambda 1.0 1.4)         => 0.52828187
 (lambda-by-pi 0.52828187 1.4)  => 0.9999999
 (lambda-by-pi 0.6019444  1.4)  => 0.8999996
@end(code)
"
  (labels
      ((closure-lambda-by-pi (pii k)
	 #'(lambda (lam)
	     (- (pi-by-lambda lam k) pii))))
    (half-div:h-div 0.0 (lambda-upper-bound k)
		    (closure-lambda-by-pi pii k))))

(export 'lambda-by-epsilon )

(defun lambda-by-epsilon (epsilon k)
"@b(Пример использования:)
@begin[lang=lisp](code)
 (epsilon-by-lambda 0.25 1.4) 0.9741614 
 (epsilon-by-lambda 0.50 1.4) 0.8990657 
 (epsilon-by-lambda 0.75 1.4) 0.7818439
 (epsilon-by-lambda 1.00 1.4) 0.6339382
 (epsilon-by-lambda 1.25 1.4) 0.4704005
 (epsilon-by-lambda 1.50 1.4) 0.3088161
 (epsilon-by-lambda 1.75 1.4) 0.1677129
 (epsilon-by-lambda 2.00 1.4) 0.0641500

 (lambda-by-epsilon 0.9741614 1.4) 0.250001
 (lambda-by-epsilon 0.8990657 1.4) 0.5000002
 (lambda-by-epsilon 0.7818439 1.4) 0.7500006
 (lambda-by-epsilon 0.6339382 1.4) 0.9999999
 (lambda-by-epsilon 0.4704005 1.4) 1.2500001
 (lambda-by-epsilon 0.3088161 1.4) 1.5000012
 (lambda-by-epsilon 0.1677129 1.4) 1.7499993
 (lambda-by-epsilon 0.0641500 1.4) 1.9999998

@end(code)
"
  (labels
      ((closure-lambda-by-epsilon (epsilon k)
	 #'(lambda (lam)
	     (- (epsilon-by-lambda lam k) epsilon))))
    (half-div:h-div 0.0 (lambda-upper-bound k)
		    (closure-lambda-by-epsilon epsilon k))))

(export '*Rmu*)

(defparameter *Rmu* 8.314 "Дж/(моль*К) универсальная газовая постоянная")

(export 'R-by-mu )

(defun R-by-mu (mu)
"@b(Описание:) Вычисляет индивидуальную газовую постоянную 
по молекулярной массе газа.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (R-by-mu 0.02895) => 287.1848
@end(code)
"
  (/ *Rmu* mu))

(export 'm-by-k-mu )

(defun m-by-k-mu (k mu)
"@b(Описание:) Вычисляет индивидуальную газовую постоянную 
по молекулярной массе газа.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (m-by-k-mu 1.4 0.02895) => 0.040405408
@end(code)
"
  (sqrt
   (* k
      (expt (/ 2 (+ k 1))
	    (/ (+ k 1)(- k 1)))
      (/ (R-by-mu mu)))))

(export 'q-by-MFR-T-P-A-k-mu )

(defun q-by-MFR-T-P-A-k-mu (MFR Temperature Pressure Area k mu)
"@b(Описание:) определяет значение газодинамической функции q.

@begin(list)
 @item(MFR - расход газа, кг/с;)
 @item(temperature  - температура остановленного потока, К;)
 @item(pressure  - полное давление, Па;)
 @item(area  - площадь м2;)
 @item(k  - коэффициент адиабаты;)
 @item(mu - молекулярная масса, кг/моль.)
@end(list)

@begin[lang=lisp](code)

@end(code)

"
  (/ (* MFR (sqrt Temperature) )
     (* (m-by-k-mu k mu) Pressure Area)))

(q-by-MFR-T-P-A-k-mu 0.5
		     (+ 400.0 273.15)
		     (* 101325.0 1.2)
		     (/ 4700.0 1000.0 1000.0)
		     1.4
		     0.02895)

(export 'a*-by-T-k-mu )

(defun a*-by-T-k-mu (Temperature k mu)
"@b(Описание:) возвращает критическую скорость.

 @b(Переменые:)
@begin(list)
 @item(temperature - температура торможения, К;) 
 @item(k  - коэффициект адиабаты cp/cv;
 @item(mu - молекулярная масса газа, кг/моль;)
@end(list)

 (a*-by-T-k-mu (+ 273 15) 1.4 0.02895) => 310.635
"
  (sqrt (/ (* 2 k (r-by-mu mu) Temperature) (+ k 1))))

(export 'a0-by-T-k-mu )

(defun a0-by-T-k-mu (temperature k mu)
"Скорость звука в заторможенном потоке.
 (a0-by-T-k-mu (+ 273 15)  1.4 0.02895) => 340.2836
"
  (sqrt (* k (r-by-mu mu) Temperature)))

(export 'a*-by-a0-k )

(defun a*-by-a0-k (a0 k)
"@b(Описание:) возвращает критическую скорость.

 @b(Переменые:)
@begin(list)
 @item(a0 - скорость звука в заторможенном потоке, м/с;) 
 @item(k  - коэффициект адиабаты cp/cv;
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (a*-by-a0-k 340.0 1.4) => 310.37613
@end(code)
"
  (* a0 (sqrt (/ 2 (+ k 1)))))

(export 'ro-by-p-t-lambda-k-mu )

(defun ro-by-p-t-lambda-k-mu (pressure temperature lam k mu)
"@b(Описание:) возвращает плотность заза

 @b(Переменые:)
@begin(list)
 @item(pressure  - полное давление, Па;)
 @item(temperature - температура торможения, К;) 
 @item(lam - относительная скорость;)
 @item(k   - коэффициент адиабаты;)
 @item(mu  - молекулярная масса, кг/моль.)
@end(list)
"
  (/ (* pressure (pi-by-lambda lam k))
     (* (r-by-mu mu) temperature (tau-by-lambda lam k))))

(export 'w-by-lambda-temperature )

(defun w-by-lambda-temperature (lam temperature k mu)
"@b(Описание:) возвращает скорость заза, м/с.

 @b(Переменые:)
@begin(list)
 @item(lam - относительная скорость;)
 @item(temperature - температура торможения, К;) 
 @item(k   - коэффициент адиабаты;)
 @item(mu  - молекулярная масса, кг/моль.)
@end(list)

@begin[lang=lisp](code)
 (w-by-lambda-temperature 0.5 (+ 400.0 273) 1.4 0.02895) => 260.08917
@end(code)
"
  (* lam (a0-by-T-k-mu temperature k mu)))
