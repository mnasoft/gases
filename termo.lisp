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
   :integration-constants (list (nth  20 lst) (nth  21 lst))))

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
газовой смеси, заданной мольными долями.

@b(Переменые:)
@begin(list)
@item(component-name - имя компонента;)
@item(mole-fraction  - мольная доля компонента.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-instance-component \"N2\" 0.78)
 (make-instance-component \"O2\" 0.22)
@end(code)
"
(defun make-instance-component (component-name mole-fraction)
  (make-instance '<component>
		 :species (gethash component-name *sp-db*)
		 :mole-fraction mole-fraction))

@export
@annot.doc:doc
"@b(Описание:) функция @b(make-instance-composition) возвращает 
газовую смесь, заданную мольными долями.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-instance-composition '((\"N2\" 0.78) (\"O2\" 0.22)))
@end(code)
"
(defun make-instance-composition (lst)
  (unless (check-spices-is-unique lst)
    (error "check-spices-is-unique=~S" (check-spices-is-unique lst) ))
  (unless (check-spices-exist-in-db lst)
    (error "check-spices-exist-in-db=~S" (check-spices-exist-in-db lst) ))
  (let ((cpm-s (make-hash-table :test #'equal)))
    (mapcar #'(lambda(el)
		(setf
		 (gethash (first el) cpm-s)
		 (make-instance-component (first el)(second el))))
	    lst)
    (let ((cmp (make-instance '<composition> :components cpm-s)))
      (unless (check-mole-fraction cmp)
	(error "check-mole-fraction=~S" (check-mole-fraction cmp) ))
      (culc-mass-fractions cmp)
      (unless (check-mass-fraction cmp)
	(error "check-mass-fraction=~S" (check-mass-fraction cmp)))
      cmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;read-formated-data

(defun read-string (str &optional (ft 's))
  (cond
    ((or (eq ft 'f) (eq ft 'e) (eq ft 'd) (eq ft 'g) (eq ft 'i))
     (if (string= "" str) 0.0 (read (make-string-input-stream str))))
    ((eq ft 's) str)
    (t (break "Format error!"))))

(defun read-record (ln-str-format str)
  (let ((st 0))
    (mapcar
     #'(lambda (el)
	 (let ((rez (string-trim " " (subseq str st (+ (second el) st)))))
	   (setf st (+ st (second el)))
	   (read-string rez (first el))))
     ln-str-format)))

(defun read-el-header (is str-1)
  (let* ((rec-1 '((s 18) (s 62)))
	 (rec-2 '((i  2) (s  8) (s  2) (f  6) (s 2) (f 6) (s 2) (f 6) (s 2) (f 6) (s 2) (f 6) (i 2) (f 13) (f 15)))
	 (rec-3 '((f 11) (f 11) (i  1) (f  5) (f 5) (f 5) (f 5) (f 5) (f 5) (f 5) (f 5) (f 17)))
	 (rec-4 '((f 16) (f 16) (f 16) (f 16) (f 16)))
	 (rec-5 '((f 16) (f 16) (f 16) (f 16) (f 16)))
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
(defparameter *sp-db* (make-hash-table :test #'equal)
  "База данных компонентов")

@export
(defun clear-db ()
  (clrhash *sp-db*))

@export
(defun init-db ()
  (clean-termo-inp)
  (clear-db)
  (mapc #'(lambda (el)
	    (let ((sp-elem (make-instance-sp el)))
	      (setf (gethash (sp-name sp-elem) *sp-db*) sp-elem)))
	(make-element-table)))

(init-db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
