;;;; air.lisp

(annot:enable-annot-syntax)

(defpackage #:gases-wet-air (:use #:cl)
	    (:nicknames "W-A" "WET-AIR")
	    (:documentation "
Файл содержит некоторые формулы для определения термодинамических
свойств влажного воздуха.

 @b(Пример использования:)
Влагосодержание воздуха при температуре 50 °C, относительной влажности 100%=1.0
и атмосферном давлении.
@begin[lang=lisp](code)
  (d-wet-air-by-temp  50.0 :fi 1.0 :p-b (* 101325 1.0)) => 85.94479, g/kg
@end(code)

Влагосодержание воздуха при температуре 150 °C, относительной влажности 100%=1.0
и  давлении равном 20 атмосфер.
@begin[lang=lisp](code)
 (d-wet-air-by-temp 150.0 :fi 1.0 :p-b (* 101325 20.0)) => 85.94479, g/kg
@end(code)
")) 

(in-package :gases-wet-air)

(defparameter *torr* 133.322 "Один торр = 133.322 Па")

@export
@annot.doc:doc
"Возвращает энтальпию влажного воздуха 

 @b(Переменые:)
@begin(list)
@item(temerature - температура возудха, °C;)
@item(d - влагосодержание, [кг воды]/[кг сухого воздуха];)
@end(list)
"
(defun J-wet-air (temerature d)
  (values
   (+
    (* 1.005 temerature)
    (* (+ 2500.0 (* 1.8 temerature)) (/ d 1000.0)))
   "kJ/kg")
  )

@export
@annot.doc:doc
"@b(Описание:) функция @b(p-wet-air-water-full) возвращает
давление насыщения водяных паров от температуры, Па.
 (Формула М.И. Фильнея. Она точнее, чем формула Г.К. Филоненко)

 @b(Переменые:)
@begin(list)
 @item(temerature - температура возудха, °C;)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (p-wet-air-water-full 100.0) =>  101357.1
 (p-wet-air-water-full 169.5) =>  801294.2
 (p-wet-air-water-full 200.0) => 1612391.4
@end(code)
 

"
(defun p-wet-air-water-full (temerature)
  (* *torr*
     (expt 10.0
	   (/ (+ 156.0 (* 8.12 temerature))
	      (+ 236 temerature)))))

@export
@annot.doc:doc
"@b(Описание:) функция @b(p-wet-air-water-full-1)

Давление насыщения водяных паров влажного воздуха от температуры. 
 (Формула Г.К. Филоненко).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (p-wet-air-water-full-1 0.0) => 558.34393
 (p-wet-air-water-full-1 100.0) 92435.086
@end(code)
"
(defun p-wet-air-water-full-1 (temerature)
  (* *torr*
     (expt 10.0
	   (+ 0.622 (/ (* 7.5 temerature) (+ 238.0 temerature))))))

@export
@annot.doc:doc
"@b(Описание:) функция @b(d-wet-air) возвращает влагосодержание
влажного воздуха, г/кг."
(defun d-wet-air (p-b p-w)
  (values (* 622.0 (/ p-w (- p-b p-w))) "g/kg"))

@export
@annot.doc:doc
"@b(Описание:) функция @b(d-wet-air-by-temp) влагосодержание влажного воздуха
от температуры и давления, "
(defun d-wet-air-by-temp (temp &key (fi 0.6) (p-b 101325.0))
  (d-wet-air p-b (* fi (p-wet-air-water-full temp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
