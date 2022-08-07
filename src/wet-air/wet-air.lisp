;;;; air.lisp

(defpackage gases/wet-air
  (:use cl)
  (:nicknames "W-A" "WET-AIR")
  (:export d-wet-air
           d-wet-air-by-temp
           p-wet-air-water-full-1
           p-wet-air-water-full
           j-wet-air
           dew-point-water)
  (:export +torr+
           )
  (:documentation
   "@b(Описание:) пакет @b(gases/wet-air) содержит
 некоторые формулы для определения термодинамических свойств влажного
 воздуха:
@begin(list)
 @item(d-wet-air, d-wet-air-by-temp - влагосодержание влажного воздуха;)
 @item(p-wet-air-water-full, p-wet-air-water-full-1 - давление насыщения водяных паров;)
 @item(dew-point-water - температура точки росы;)
 @item(j-wet-air - энтальпия влажного воздуха. )
@end(list)

 @b(Пример использования:) 

 Влагосодержание воздуха при температуре 50[°C], относительной
 влажности 100%=1.0 и атмосферном давлении.

 @begin[lang=lisp](code)
  (d-wet-air-by-temp  50.0 :fi 1.0 :p-b (* 101325 1.0)) => 85.94479, g/kg
@end(code)

 Влагосодержание воздуха при температуре 150 [°C], относительной влажности 100%=1.0
и  давлении равном 20 атмосфер.
@begin[lang=lisp](code)
 (d-wet-air-by-temp 150.0 :fi 1.0 :p-b (* 101325 20.0)) => 85.94479, g/kg
@end(code)
")) 

(in-package gases/wet-air)

(defconstant +torr+ 133.322
  "@b(Описание:) константа @b(+torr+) содержит значение одного торра.
1.0 [Торр] = 133.322 [Па]")

(defun partial-pressure (x pressure)
  "@b(Описание:) функция @b(partial-pressure) возвращает парциальное
  давление компонента, заданного в составе газовой смеси мольной
  долей."
  (* x pressure))

(defun J-wet-air (temerature d) 
"@b(Описание:) функция @b(J-wet-air) возвращает энтальпию влажного
 воздуха.

 @b(Переменые:)
@begin(list)
 @item(temerature - температура возудха, [°C];)
 @item(d - влагосодержание, [кг воды]/[кг сухого воздуха].)
@end(list)"
  (values
   (+
    (* 1.005 temerature)
    (* (+ 2500.0 (* 1.8 temerature)) (/ d 1000.0)))
   "kJ/kg"))

(defun p-wet-air-water-full (temerature)
  "@b(Описание:) функция @b(p-wet-air-water-full) возвращает
давление насыщения водяных паров от температуры, [Па].

 (Формула М.И. Фильнея) Она точнее, чем формула Г.К. Филоненко.

 @b(Переменые:)
@begin(list)
 @item(temerature - температура возудха, [°C].)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (p-wet-air-water-full 100.0) => 101357.1, \"Па\"
 (p-wet-air-water-full 169.5) =>  801294.2, \"Па\"
 (p-wet-air-water-full 200.0) => 1612391.4, \"Па\"
@end(code)
"
  (values
   (* +torr+
      (expt 10.0
	    (/ (+ 156.0 (* 8.12 temerature))
	       (+ 236 temerature))))
   "Па"))


(defun p-wet-air-water-full-1 (temerature)
  "@b(Описание:) функция @b(p-wet-air-water-full-1) возвращает
 давление насыщения водяных паров влажного воздуха от температуры,
 [Па].

 (Формула Г.К. Филоненко).

 @b(Переменые:)
@begin(list)
 @item(temerature - температура возудха, [°C].)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (p-wet-air-water-full-1 0.0) => 558.34393
 (p-wet-air-water-full-1 100.0) 92435.086
@end(code)
"
  (values
   (* +torr+
      (expt 10.0
	    (+ 0.622 (/ (* 7.5 temerature) (+ 238.0 temerature)))))
   "Па"))

(defun d-wet-air (barometric-pressure water-pressure)
  "@b(Описание:) функция @b(d-wet-air) возвращает влагосодержание
 влажного воздуха, [г/кг].

 @b(Переменые:)
@begin(list)
 @item(barometric-pressure - барометрическое давление, [Па];)
 @item(water-pressure - парциальное давление паров воды, [Па].)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (d-wet-air 101325.0 5000.0) => 32.28653, \"g/kg\"
@end(code)
"
  (values
   (* 622.0
      (/ water-pressure
         (- barometric-pressure water-pressure))) "g/kg"))

(defun d-wet-air-by-temp (temerature &key (fi 0.6) (p-b 101325.0))
  "@b(Описание:) функция @b(d-wet-air-by-temp) возвращает
 влагосодержание влажного воздуха от температуры и давления, [г/кг].

 @b(Переменые:)
@begin(list)
 @item(temerature - температура влажного воздуха, [°C];)
 @item(fi - относительная влажность, [доли];)
 @item(p-b - давленрие влажного воздуха, [Па].)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (d-wet-air-by-temp 20.0 :fi 0.6) => 8.728282, \"g/kg\"
 (d-wet-air-by-temp 20.0 :fi 0.6 :p-b 100000.0) => 8.845576, \"g/kg\"
@end(code)
"
  (d-wet-air p-b (* fi (p-wet-air-water-full temperature))))

(defun dew-point-water (x pressure)
  "@b(Описание:) функция @b(dew-point-water) возвращает температуру
 точки росы смеси газов по воде, [°C].

 @b(Переменые:)
@begin(list)
 @item(x - мольная доля воды в составе смеси;)
 @item(pressure - давление смеси, [Па].)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dew-point-water 0.0025 (* 3.6 1000 1000)) => 43.830765
 (dew-point-water 0.0006 (* 3.6 1000 1000)) => 18.733126
@end(code)
"
  (let ((p (partial-pressure x pressure)))
    (half-div:h-div
     0.0
     150.0
     #'(lambda (temperature)
         (- p (p-wet-air-water-full temperature))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun p-ws (T-K)
  "Давление насыщенного пара в зависимости от температуры.
На основе отраслевых формул Международной ассоциации по свойствам воды
и водяного пара, 1997 г. (IAPWS Industrial Formulation 1997) для
термодинамических свойств воды и водяного пара (IAPWS-IF97).  ИСО
2314:2009 c.76.

Рзмерность - [бар а.]

 @b(Пример использования:)
@begin[lang=lisp](code)
(p-ws (+ 273.15 ))      =>   0.006111850299232557d0
(p-ws (+ 373.15 ))      =>   1.0167124142699038d0 
(p-ws (+ 273.15 374.0)) => 311.73666715222157d0
@end(code)
"
  (let ((a0 -1.7139186035d+01)
        (a1  3.0961739394d-01)
        (a2 -2.1566592224d-03)
        (a3  6.7915334500d-06)
        (a4 -6.7999635030d-09)
        (a5 -1.0518480668e-11)
        (a6  2.1477192075d-14)
        )
    (+ a0
       (* a1 T-K)
       (* a2 T-K T-K)
       (* a3 T-K T-K T-K)
       (* a4 T-K T-K T-K T-K)
       (* a5 T-K T-K T-K T-K T-K)
       (* a6 T-K T-K T-K T-K T-K T-K))))
    

