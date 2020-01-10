;;;; gasodinamic-func.lisp
(in-package #:gases)

;;;;
;;;; Число Маха - отношение скорости потока к местной скорости звука.
;;;; Приведенная скорость (коэффициент скорости) - отношение скорости потока к критической скорости.
(defun Mah-by-lambda (lam k)
  "@b(Пример использования:)
@begin[lang=lisp](code)
 (Mah-by-lambda 1.8212 1.4) 
@end(code)
"
  (/
   (* lam
      (sqrt
       (/ 2 (+ k 1))))
   (sqrt
    (- 1
       (*
	(/ (- k 1)
	   (+ k 1))
	(expt lam 2))))))

(defun Lambda-by-mah (Mah k)
    "@b(Пример использования:)
@begin[lang=lisp](code)
 (Lambda-by-mah 51.5 1.4) 
@end(code)
"
  (/ (* Mah
	(sqrt
	 (/ (+ k 1) 2)))
     (sqrt
      (+ 1
	(*
	 (/ (- k 1) 2)
	 (expt Mah 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'tau-by-lambda)
(defun tau-by-lambda (lam k)
  "@b(Описание:) Газодинамическая функция идеального газа.
Возвращает отношение температуры в данном сечении к температуре заторможенного потока.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (tau-lambda 1. 1.4)
@end(code)
"
  (- 1 (* (/ (- k 1) (+ k 1)) (expt lam 2))))

(export '1/tau-by-mah)
(defun 1/tau-by-mah (mah k)
  "@b(Описание:) Газодинамическая функция идеального газа.
Возвращает отношение температуры в данном сечении к температуре заторможенного потока.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let* ((l 1.5)
       (m(mah-by-lambda  l 1.4)))
 (values l  m (tau-lambda l 1.4) (/ (1/tau-by-mah m 1.4))))
 
@end(code)
"
  (+ 1 (* (- k 1) 1/2 (expt mah 2))))

(defun pi-by-lambda (Lam k)
  "@b(Пример использования:)
@begin[lang=lisp](code)
 (pi-by-lambda 1.8212 1.4) => 0.059809975
@end(code)
"
  (expt
   (- 1
      (*
       (/
	(- k 1)
	(+ k 1))
       (expt Lam 2)))
   (/ k (- k 1))))

(defun epsilon-by-lambda (Lam k)
    "@b(Пример использования:)
@begin[lang=lisp](code)
 (epsilon-by-lambda 1.8212 1.4) => 0.1337417
@end(code)
"
  (expt
   (- 1
      (*
       (/
	(- k 1)
	(+ k 1))
       (expt Lam 2)))
   (/ 1 (- k 1))))

(defun q-by-lambda (lam k)
  "@begin[lang=lisp](code)
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

(defun lambda-upper-bound (k)
  "Определяет верхнюю границу для диапазода значений lambda."
  (sqrt (/ (+ k 1) (- k 1))))

(defun lambda-by-q (q k)
  "lambda-by-q - функция обранная q-by-lambda при постоянном значении k.
Функция возвращает два значения:
- первое для докритической области;
- второе для закритической области.
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


(defun lambda-by-tau (tau k)
  "
@begin[lang=lisp](code)
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

(defun lambda-by-epsilon (epsilon k)
  (labels
      ((closure-lambda-by-epsilon (epsilon k)
	 #'(lambda (lam)
	     (- (pi-by-lambda lam k) epsilon))))
    (half-div:h-div 0.0 (lambda-upper-bound k)
		    (closure-lambda-by-pi epsilon k))))
