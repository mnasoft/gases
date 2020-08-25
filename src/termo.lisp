;;;; termo.lisp

(in-package :gases)

(annot:enable-annot-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    make-instance                                                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-instance-sp-rec (lst)
  (make-instance
   '<sp-rec>
   :temperature-range     (list (nth  0 lst) (nth  1 lst))
   :number-coeff          (nth  2 lst)
   :polynomial-exponents  (subseq lst 3 (+ 3 (nth  2 lst)))
   :H_298.15-H-0          (nth  11 lst)
   :coefficients          (subseq lst 12 (+ 12 (nth  2 lst)))
   :integration-constants (list (nth  19 lst)  (nth  20 lst))))

(defun make-instance-sp (lst)
  "Создает оъект <sp> "
  (make-instance
   '<sp>
   :name (first lst)
   :comments (second lst)
   :number-temperature-intervals (third lst)
   :reference-date-code (fourth lst)         
   :chemical-formula (list (list (nth  4 lst) (nth  5 lst))
			   (list (nth  6 lst) (nth  7 lst))
			   (list (nth  8 lst) (nth  9 lst))
			   (list (nth 10 lst) (nth 11 lst))
			   (list (nth 12 lst) (nth 13 lst)))            
   :phase                        (nth 14 lst)
   :molar-mass                   (nth 15 lst)
   :heat-formation               (nth 16 lst)
   :reccords               (mapcar #'make-instance-sp-rec (car(last lst)))))

@export
@annot.doc:doc
"@b(Описание:) функция @b(make-instance-component) возвращает компонент
газовой смеси, заданной мольными или массовыми долями. По умолчанию
поределяется через мольную долю.

@b(Переменые:)
@begin(list)
@item(component-name - имя компонента;)
@item(fraction - доля компонента мольная или массовая;)
@item(fraction-type - тип доли. :mole задает мольную долю; :mass - массовую.
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-instance-component \"N2\" 0.78)
 (make-instance-component \"O2\" 0.22)
 (make-instance-component \"N2\" 0.78 :mass)
 (make-instance-component \"O2\" 0.78 :mass)
@end(code)
"
(defun make-instance-component (component-name fraction &optional (fraction-type :mole))
  (ecase fraction-type
  (:mole (make-instance '<component>
		 :species (get-sp component-name)
		 :mole-fraction fraction))
  (:mass (make-instance '<component>
		 :species (get-sp component-name)
		 :mass-fraction fraction))))

@export
@annot.doc:doc
"@b(Описание:) функция @b(make-instance-composition) возвращает 
газовую смесь, заданную мольными долями.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-instance-composition '((\"N2\" 0.78) (\"O2\" 0.22)))
 (make-instance-composition '((\"N2\" 0.78) (\"O2\" 0.22)) :mole)
 (make-instance-composition '((\"N2\" 0.78) (\"O2\" 0.22)) :mass)
@end(code)
"
(defun make-instance-composition (lst &optional (fraction-type :mole))
  (unless (check-spices-is-unique lst)
    (error "Spices is not unique=~S" (check-spices-is-unique lst) ))
  (unless (check-spices-exist-in-db lst)
    (error "Some spices is not exist in db=~S" (check-spices-exist-in-db lst) ))
  (let ((cpm-s (make-hash-table :test #'equal)))
    (mapcar #'(lambda(el)
		(setf
		 (gethash (first el) cpm-s)
		 (make-instance-component (first el)(second el) fraction-type)))
	    lst)
    (let ((cmp (make-instance '<composition> :components cpm-s)))
      (ecase fraction-type
	(:mole
	 (unless (check-mole-fraction cmp)
	   (error "Mole fraction summ=~S not equal 1.0"
		  (molar-fraction-summ cmp)))
	 (culc-mass-fractions cmp))
	(:mass
	 (unless (check-mass-fraction cmp)
	   (error "Mass fraction summ=~S not equal 1.0"
		  (mass-fraction-summ cmp)))
	 (culc-molar-fractions cmp)))
    cmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;read-formated-data

(defun read-string (str &optional (ft 's))
  (cond
    ((or (eq ft 'f) (eq ft 'e) (eq ft 'g) (eq ft 'i))
     (if (string= "" str) 0.0 (read (make-string-input-stream str))))
    ((eq ft 'd)
     (if (string= "" str) 0.0d0
	 (read
	  (make-string-input-stream
	   (substitute #\D #\E str)))))
    ((eq ft 's)    str)
    ((eq ft '_)    nil)
    ((eq ft 'f->d)
     (if (string= "" str) 0.0d0
	 (read (make-string-input-stream
		(concatenate 'string str "d0")))))
    (t (break "Format error!"))))

(defun read-record (ln-str-format str)
  (let ((st 0))
    (remove-if #'null
	       (mapcar
		#'(lambda (el)
		    (let ((rez (string-trim " " (subseq str st (+ (second el) st)))))
		      (setf st (+ st (second el)))
		      (read-string rez (first el))))
		ln-str-format))))

(defun read-el-header (is str-1)
  (let* ((rec-1 '((s 16) (_ 2) (s 62)))
	 (rec-2 '((i  2) (_ 1) (s  6) (_ 1)
		  (s 2) (f 6)
		  (s 2) (f 6)
		  (s 2) (f 6)
		  (s 2) (f 6)
		  (s 2) (f 6) (i 2) (f->d 13) (f->d 15)))
	 (rec-3 '((f 11) (f 11) (i  1) (f  5) (f 5) (f 5) (f 5) (f 5) (f 5) (f 5) (f 5) (_ 2) (f->d 15)))
	 (rec-4 '((d 16) (d 16) (d 16) (d 16) (d 16)))
	 (rec-5 '((d 16) (d 16) (_ 16) (d 16) (d 16)))
	 (r-1 (read-record rec-1 str-1))
	 (r-2 (read-record rec-2 (read-line is)))
	 (n-row (first r-2))
	 (t-int nil))
    (when (< 0 n-row)
      (dotimes (i n-row)
	(push 
	 (append (read-record rec-3 (read-line is))
		 (read-record rec-4 (read-line is))
		 (read-record rec-5 (read-line is)))
	 t-int)))
    (when (= 0 n-row)
      (push (read-record rec-3 (read-line is)) t-int))
    (append r-1 r-2 (list (reverse t-int)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clean-termo-inp ()
  "@b(Описание:) функция @b(clean-termo-inp) выполняет очистку формата
ввода данных от комментариев.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (clean-termo-inp)
@end(code)
"
  (with-open-file (os (namestring (asdf:system-relative-pathname :gases "data/termo.inp.clean")) :direction :output :if-exists :supersede)
    (with-open-file (is (namestring (asdf:system-relative-pathname :gases "data/termo.inp"))     :direction :input)
      (do ((line (read-line is nil 'eof) (read-line is nil 'eof))
	   (str-format "~A"))
	  ((eql line 'eof))
	(unless
	    (or (string= "" (string-trim " " line))
		(string= "!" (subseq line 0 1))
		(string= "#" (subseq line 0 1))
		(string= "END PRODUCTS" (string-trim " " line))
		(string= "END REACTANTS" (string-trim " " line)))
	  (format os str-format line)
	  (setf str-format "~%~A"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.doc:doc
"@b(Описание:) функция @b(make-element-table)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-element-table)
@end(code)
"
(defun make-element-table ()
  (let ((rez nil)
	(rez-lst nil))
    (with-open-file (is (namestring (asdf:system-relative-pathname :gases "data/termo.inp.clean")) :direction :input)
      (read-line is nil 'eof)
      (read-line is nil 'eof)
      (do ((line (read-line is nil 'eof) (read-line is nil 'eof)))
	  ((eql line 'eof))
	(setf rez (read-el-header is line))
	(push rez rez-lst)))
    rez-lst))

(clean-termo-inp)
(make-element-table)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"Возвращает базу данных компонентов."
(defun get-db () *sp-db*)

@export
(defun clear-db ()
  (clrhash (get-db)))

@export
@annot.doc:doc
"Возвращает компонент по имени."
(defun get-sp (name) (gethash name (get-db)))

(defun (setf get-sp) (value name)
  (setf (gethash name (get-db)) value))

@export
(defun init-db ()
  (clean-termo-inp)
  (clear-db)
  (mapc #'(lambda (el)
	    (let ((sp-elem (make-instance-sp el)))
	      (setf (get-sp (sp-name sp-elem)) sp-elem)))
	(make-element-table)))

(block init-db
  (init-db)
  (map nil
       #'(lambda (el)
	   (setf (gethash el *not-combasted-sp*) (get-sp el)))
       *not-combasted-sp-names*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
