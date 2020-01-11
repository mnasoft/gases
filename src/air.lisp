;;;; air.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

(defparameter *torr* 133.322 "Один торр = 133.322 Па")

(defun J-wet-air (temerature d)
  (values
   (+
    (* 1.005 temerature)
    (* (+ 2500.0 (* 1.8 temerature)) (/ d 1000.0)))
   "kJ/kg")
  )

(defun p-wet-air-water-full (temerature)
  "Давление насыщения водяных паров влажного воздуха от температуры. 
Формулу предложил М.И. Фильней. Более точная"
  (* *torr*
     (expt 10.0
	   (/ (+ 156.0 (* 8.12 temerature))
	      (+ 236 temerature)))))

(defun p-wet-air-water-full-1 (temerature)
  "Давление насыщения водяных паров влажного воздуха от температуры. 
Формулу предложил Г.К. Филоненко"
  (* *torr*
     (expt 10.0
	   (+ 0.622 (/ (* 7.5 temerature) (+ 238 temerature))))))

(/ (p-wet-air-water-full 60.0) 133.3)

(defun d-wet-air (p-b p-w)
  "Влагосодержание влажного воздуха"
  (values (* 622.0 (/ p-w (- p-b p-w))) "g/kg"))


(defun d-wet-air-by-temp (temp &key (fi 0.6) (p-b 101325.0))
  "Влагосодержание влажного воздуха от температуры и давления"
  (d-wet-air p-b (* fi (p-wet-air-water-full temp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Пример использования:
;;;; Влагосодержание воздуха при температуре 50 °C, относительной влажности 100%=1.0 и атмосверном давлении 
;;;; (d-wet-air-by-temp 50.0 :fi 1 :p-b (* 101325 1.0))
;;;; (d-wet-air-by-temp 150.0 :fi 1 :p-b (* 101325 20.0))
