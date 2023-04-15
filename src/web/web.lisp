;;;; web.lisp
<<<<<<< HEAD

(in-package :cl-user)

=======
>>>>>>> a6f74355141d554fc1cef78ac6c7eb24957b6563
(defpackage :gases/web
  (:use #:cl)
  (:export html-out
           make-table-periodic
           filter-not-checked-elements 
           query-map 
           query-item 
           )
  (:documentation
   "@b(Описание:) пакет @b(gases/web) создается для отбора химических
веществ на основании состава их молекул по:
@begin(list)
 @item(присутствию атома в сосаве молекулы;)
 @item(присутствию точного большего или меньшего количества атомов в составе молекулы;)
@end(list)

Состояние проекта - пока не дописан.

 @b(Пример использования:)
@begin[lang=lisp](code)
(with-open-file (stream \"~/table-periodic.html\" :direction :output)
    (html-out (make-table-periodic) stream))
@end(code)

"))

(in-package :gases/web)

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
  "@b(Описание:) функция @b(element-color) возвращает список, состоящий из
трёх элементов:
@begin(list)
 @item(ключ;)
 @item(код цвета;)
 @item(описание.)
@end(list)
или NIL, если в периодической системе элементов этого элемента нет.

 @b(Переменые:)
@begin(deflist)
@term(el-number) @def(номер єлемента в периодической системе єлементов)
@end(deflist)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (element-color 1) => (:C06 \"#a0ffa0\" \"Другие неметаллы (16-я (VI) группа -- халькогены)\")
 (element-color 2) => (:C08 \"#c0ffff\" \"Благородные газы\")
 (element-color 3) => (:C01 \"#f66\" \"Щёлочные металлы\")
 (element-color 4) => (:C02 \"#ffdead\" \"Щёлочноземельные металлы\")
 (element-color 5) => (:C05 \"#cc9\" \"Полуметаллы -- металлоиды\")
 (element-color 6) => (:C06 \"#a0ffa0\" \"Другие неметаллы (16-я (VI) группа -- халькогены)\")
 (element-color 9) => (:C07 \"#ff9\" \"Галогены\")
 (element-color 200) => NIL
@end(code)
"
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


(export 'html-out )
(defgeneric html-out (obj stream)
  (:documentation "Вывод объекта obj в поток stream."))

(defmethod html-out ((el t) s)
  (when el (format s "~A" el)))

(defmethod html-out ((el ELEMENTS:ELEMENT) s)
  "@b(Описание:) метод @b(html-out)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (html-out (elements:atomic-number-element 12) nil) 
@end(code)
"
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
					:max "100"))))))))

(defmethod html-out ((mm math/matr:<matrix>) s)
  "Новая версия"
  (let ((o-str (make-string-output-stream)))
    (format o-str "~%<table>")
    (loop :for i :from 0 :below (math/matr:rows mm) :do
      (let ((rr (math/matr:row mm i)))
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

(defun make-table-periodic ()
  (let ((tbl-perodic-long
	  (make-instance 'math/matr:<matrix>
			 :dimensions '(10 19)
			 :initial-element nil)))
      
<<<<<<< HEAD
    (loop :for i :from 0 :below (math:rows tbl-perodic-long) :do
      (loop :for j :from 0 :below (math:cols tbl-perodic-long) :do
=======
    (loop :for i :from 0 :below (math/matr:rows tbl-perodic-long) :do
      (loop :for j :from 0 :below (math/matr:cols tbl-perodic-long) :do
>>>>>>> a6f74355141d554fc1cef78ac6c7eb24957b6563
	(setf (math/matr:mref tbl-perodic-long i j) nil)))
  
    (loop :for i :from 1 :to 112 :do
      (let ((t-p-g (period-group-long i)))
	(when (and t-p-g (eq :M (first t-p-g)))
	  (setf (math/matr:mref tbl-perodic-long
			   (1- (second t-p-g))
			   (1- (third  t-p-g)))
		(elements:atomic-number-element i)))
	(when (and t-p-g (eq :LA (first t-p-g)))
	  (setf (math/matr:mref tbl-perodic-long
			   (+ (second t-p-g) 1)
			   (1- (third  t-p-g)))
		(elements:atomic-number-element i)))
	(when (and t-p-g (eq :AC (first t-p-g)))
	  (setf (math/matr:mref tbl-perodic-long
			   (+ (second t-p-g) 2)
			   (1- (third  t-p-g)))
		(elements:atomic-number-element i)))))
    tbl-perodic-long))

(defmethod html-out ((sp gases/db:<sp>) s)
  "Короткая версия для вывода элемента"
  (format s "~A"
	  (cl-who:with-html-output-to-string (o-str nil :indent t)
	    (:div
	     (:table :bordercolor "\\#ff0" :border "2"
		     (:tr
		      (:td (cl-who:str (gases/db:<sp>-name       sp)))
		      (:td (cl-who:str (gases/db:<sp>-molar-mass sp)))
		      (:td (cl-who:str (gases/db:<sp>-comments   sp)))))))))

(defun filter-not-checked-elements (lst)
  (remove-if
   #'(lambda (el)
       (let ((al (cdr el)))
	 (and (string= "" (cdr (assoc "l-e-b" al :test #'string=)))
	      (string= "" (cdr (assoc "num"   al :test #'string=))))))
   lst))

(defun query-map (lst)
  (eval
   `(gases/core:find-by-atoms
     ,(cons 'and (mapcar #'query-item
			 lst)))))

(defun query-item (lst)
  (let ((element (string-upcase (car lst)))
	(ch-box  (cdr (assoc "ch-box" (cdr lst) :test #'string=)))
	(l-e-b   (cdr (assoc "l-e-b"  (cdr lst) :test #'string=)))
	(num     (cdr (assoc "num"    (cdr lst) :test #'string=)))
	)
    (list element ch-box l-e-b num)
    (when (and ch-box
	       l-e-b
	       num
	       (parse-integer num :junk-allowed t)
	       (string= ch-box "on")
	       (member l-e-b '("<" ">" "=") :test #'string=)
	       (<= 1 (parse-integer num :junk-allowed t)))
      `(gases/core:q-of
	,element
	,(cond ((string= l-e-b ">") '>=)
	       ((string= l-e-b "=") '=)
	       ((string= l-e-b "<") '<=))
	,(parse-integer num :junk-allowed t)))))

;;;; (gases/core:find-by-atoms (and (gases/core:q-of "H" >= 2) (gases/core:q-of "H" <= 8) (gases/core:q-of "C" = 1))) 

;;;; (html-out (gases/db:get-sp "C2H5OH") t)
;;;; (gases/db:<sp>-molar-mass
;;;; (gases/core:find-by-atoms (and (gases/core:q-of "H" >= 2) (gases/core:q-of "H" <= 8) (gases/core:q-of "C" = 1))) 
;;;; (html-out (make-table-periodic) t)
;;;; (period-group-long 1)
;;;; (elements:element-name (elements:atomic-number-element 12)) ; => "Magnesium"
;;;; (type-of (elements:atomic-number-element 12)) ; => ELEMENTS:ELEMENT
;;;; (let ((o-str (make-string-output-stream))) (get-output-stream-string o-str)))

;;;; (query-map '(("Ne" ("ch-box" . "on") ("l-e-b" . "=") ("num" . "1"))))

;;;; (query-item '("Ne" ("ch-box" . "on") ("l-e-b" . "=") ("num" . "1")))
;;;; (gases/core:q-of "Ne" = 1)


(defparameter *bag* '(0 1000 0))

(defun silver ()
  (setf (second *bag*) (- (second *bag*) 5))
  (setf (first  *bag*) (+ (first  *bag*) 3))
  (setf (third  *bag*) (+ (third  *bag*) 1))
  *bag*)

(defun gold ()
  (setf (first  *bag*) (- (first  *bag*) 2))
  (setf (second *bag*) (+ (second *bag*) 3))
  (setf (third  *bag*) (+ (third  *bag*) 1))
  *bag*)


(silver)
(gold)

(with-open-file (stream "~/table-periodic.html" :direction :output)
    (html-out (make-table-periodic) stream))
