;;;; ./src/db/db.lisp

(defpackage :gases/db
  (:use cl)
  (:export <sp-rec> <sp-rec>-polynomial-exponents
           <sp-rec>-temperature-range <sp-rec>-number-coeff
           <sp-rec>-h_298.15-h-0 <sp-rec>-coefficients
           <sp-rec>-integration-constants)
  (:export <sp> <sp>-name <sp>-comments
           <sp>-number-temperature-intervals <sp>-reference-date-code
           <sp>-chemical-formula <sp>-phase <sp>-molar-mass
           <sp>-heat-formation <sp>-reccords)
  (:export get-db clear-db get-sp init-db dump dump+ dump+d->e)
  (:export *not-combasted-sp-names* *not-combasted-sp* *sp-db* *str-db*))

(in-package :gases/db)

(defgeneric dump (reference stream)
  (:documentation "@b(Описание:) метод @b(dump) сбравывает символьное
 представление reference в символьный поток stream."))

(defclass <sp-rec> nil
  ((temperature-range
    :accessor <sp-rec>-temperature-range
    :initarg :temperature-range
    :documentation
    "Temperature range (cols 2-21, 2x 10.3f). 
The minimum and maximum bounds for the current temperature interval.
Units, K.")
   (number-coeff
    :accessor <sp-rec>-number-coeff :initarg
    :number-coeff :initform 7 :documentation
    "Number of coefficients (col 23, int). 
This is always 7 in this data (though the database format supports 8,
see section Redundancy).")
   (polynomial-exponents
    :accessor <sp-rec>-polynomial-exponents
    :initarg :polynomial-exponents
    :initform '(-2 -1 0 1 2 3 4)
    :documentation
    "Polynomial exponents (cols 24-63, 8x 5.1f). 
These are always [-2, -1, 0, 1, 2, 3, 4] in this data.")
   (h_298.15-h-0
    :accessor <sp-rec>-h_298.15-h-0 :initarg
    :h_298.15-h-0 :documentation
    "{H(298.15) - H(0)} (cols 66-80, 15.3f). 
This is the difference between the heat of formation at the enthalpy at T = 0 K.")
   (coefficients
    :accessor <sp-rec>-coefficients :initarg
    :coefficients :initform '(0 0 0 0 0 0 0)
    :documentation
    "Coefficients 1-5 (cols 1-80, 5x 16.8f). 
Coefficients 6-8 (cols 1-48, 3x 16.8f). 
The 8th is not used in this data (see section Redundancy).")
   (integration-constants
    :accessor <sp-rec>-integration-constants
    :initarg :integration-constants
    :initform '(0 0)
    :documentation
    "Integration constants (cols 49-80, 2x 16.8f). 
Used in evaluation of enthalpy and temperature-dependent 
component of entropy, respectively."))
  (:documentation
   "Представляет данные для расчета теплоемкости, энтальпии и 
энторопии в определенном диапазоне температур."))

(defmethod print-object :before ((x <sp-rec>) s)
  (format s
	  "#<sp-rec>(
 <sp-rec>-temperature-range=~S
 <sp-rec>-number-coeff=~S
 <sp-rec>-polynomial-exponents=~S
 <sp-rec>-h_298.15-h-0=~S
 <sp-rec>-coefficients=~S
 <sp-rec>-integration-constants=~S~%"
	  (<sp-rec>-temperature-range x)
	  (<sp-rec>-number-coeff x)
	  (<sp-rec>-polynomial-exponents x)
	  (<sp-rec>-h_298.15-h-0 x)
	  (<sp-rec>-coefficients x)
	  (<sp-rec>-integration-constants x)))

(defmethod print-object         ((x <sp-rec>) s) (format s "" ))

(defmethod print-object :after  ((x <sp-rec>) s) (format s ")" ))

(defclass <sp> ()
  ((name
    :accessor <sp>-name :initarg :name :initform ""
    :documentation
    "Species name/formula (cols 1-15, 15str).  This serves as an
ID. Note that 'l' is represented by L and condensed phases designated
as α, β, γ or δ are renamed a, b, c or d due to ASCII limitations.")
   (comments
    :accessor <sp>-comments :initarg :comments :initform ""
    :documentation
    "Comments (cols 16-80, 65str). These include references in the
format of author, year or page and date in the case of TRC tables.
When heat of formation is taken from a separate reference, this is
included as Hf:<ref>. Reference elements or sp used for heat of
formation calculations are indicated by Ref-Elm or Ref-Sp.")
   (number-temperature-intervals
    :accessor <sp>-number-temperature-intervals
    :initarg :number-temperature-intervals
    :initform 0
    :documentation
    "Number of temperature intervals (col 2, 2int).")
   (reference-date-code
    :accessor <sp>-reference-date-code :initarg
    :reference-date-code :initform ""
    :documentation
    "Reference-Date code (cols 4-9, 6str). This includes a character
 indicating a general reference followed by a date (e.g. g indicates
 that NASA Glenn was the source of significant work in deriving the
 data and 10/96 indicates the month/year).")
   (chemical-formula
    :accessor <sp>-chemical-formula :initarg
    :chemical-formula :initform "" :documentation
    "Chemical formula (cols 11-50, 2str + 6.2f). This is a set of 5
 element/atom, number pairs. In the vast majority of cases the numbers
 are integers but in some cases they are non-integer, so floats are
 used.")
   (phase
    :accessor <sp>-phase :initarg :phase :initform ""
    :documentation
    "Phase (col 52, int). Zero for gas, nonzero for condensed
    phases.")
   (molar-mass
    :accessor <sp>-molar-mass :initarg :molar-mass
    :initform "" :documentation
    "Molar mass (cols 53-65, 13.5f). Originally labelled molecular
    weight
 (in units g/mol).")
   (heat-formation
    :accessor <sp>-heat-formation :initarg :heat-formation :initform ""
    :documentation
    "Heat of formation (cols 66-80, 13.5f).  In the case of condensed
 species this is actually an assigned enthalpy
 (equivalent to the heat of formation at 298.15 K). Units J/mol.")
   (reccords
    :accessor <sp>-reccords :initarg :reccords :initform ""
    :documentation
    "Список из нескольких элементов класса sp-rec"))
  (:documentation
   "Представляет молекулу вещества (Species name/Formula).  Данные для
 элементов взяты из базы данных NASA
 (см. https://www.grc.nasa.gov/www/CEAWeb/)"))

(defmethod print-object :before ((x <sp>) s)
  (format s
	  "#<sp>(
 <sp>-name=~S
 <sp>-comments=~S
 <sp>-number-temperature-intervals=~S
 <sp>-reference-date-code=~S
 <sp>-chemical-formula=~S
 <sp>-phase=~S
 <sp>-molar-mass=~S
 <sp>-heat-formation=~S
 <sp>-reccords=~S~%"
	  (<sp>-name x)
	  (<sp>-comments x)
	  (<sp>-number-temperature-intervals x)
	  (<sp>-reference-date-code x)
	  (<sp>-chemical-formula x)
	  (<sp>-phase x)
	  (<sp>-molar-mass x)
	  (<sp>-heat-formation x)
	  (<sp>-reccords x)))

(defmethod print-object         ((x <sp>) s) (format s "" ))

(defmethod print-object :after  ((x <sp>) s) (format s ")" ))

(defparameter *not-combasted-sp-names* '("N2" "O2" "H2O" "CO2" "SO2" "SO3" "He" "Ar" "Kr" "Xe" "Rd")
  "Список имен компонентов неучаствующих в реакции окисления кислородом O2.")

(defparameter *not-combasted-sp* (make-hash-table :test #'equal)
  "Хеш-таблица ключ - имя; значение - объект класса <sp>,
 неучаствующих в реакции окисления кислородом O2.")

(defparameter *sp-db* (make-hash-table :test #'equal)
  "База данных компонентов")

(defparameter *str-db* nil
  "Строковое представление базы данных.
 Содержимое файла data/termo.inp")

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

(defun make-element-table ()
  "@b(Описание:) функция @b(make-element-table)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-element-table)
@end(code)
"
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

(defun get-db ()
  "Возвращает базу данных компонентов."
  *sp-db*)

(defun clear-db ()
  (clrhash (get-db)))

(defun get-sp (name)
  "Возвращает компонент по имени."
  (gethash name (get-db)))

(defun (setf get-sp) (value name)
  (setf (gethash name (get-db)) value))

(defun init-db ()
  (clean-termo-inp)
  (clear-db)
  (mapc #'(lambda (el)
	    (let ((sp-elem (make-instance-sp el)))
	      (setf (get-sp (<sp>-name sp-elem)) sp-elem)))
	(make-element-table)))

(block init-db
  (init-db)
  (map nil
       #'(lambda (el)
	   (setf (gethash el *not-combasted-sp*) (get-sp el)))
       *not-combasted-sp-names*))

(defun append-some-value-to-length (new-len value lst)
  (loop :for i :from 0 :below new-len
	:collect
	(let ((l (nth i lst)))
	  (if  l l value))))

(defmethod dump ((sp <sp>) s)
  "@b(Описание:) метод @b(dump) выполняет вывод объекта sp в поток s.
 Вывод должен осуществляться в форме пригодной для последующего считывания 
 в формате TermoBuild.
"
  (labels ((rec () (first (<sp>-reccords sp))))
    (format s "~16A  ~62A~%" (<sp>-name sp) (<sp>-comments sp))
    (format s "~2D ~6A ~{~2A~6,2F~}~2D~13,7f~15,3f~%"
	    (<sp>-number-temperature-intervals sp)
	    (<sp>-reference-date-code sp) 
	    (apply #'append (<sp>-chemical-formula sp))
	    (<sp>-phase sp) 
	    (<sp>-molar-mass sp) 
	    (<sp>-heat-formation sp))
    (when (/= 0 (<sp>-number-temperature-intervals sp))
      (map nil #'(lambda (el)(dump el s))
	   (<sp>-reccords sp)))
    (when (= 0 (<sp>-number-temperature-intervals sp))
      (format s "~{~11,3f~}~1D~{~5,1F~}  ~15,3F~%" ;
	      (<sp-rec>-temperature-range (rec))
	      (<sp-rec>-number-coeff (rec))
	      (append-some-value-to-length
	       8 0.0 (<sp-rec>-polynomial-exponents (rec))) ;; Проверить считыватель
	      (<sp-rec>-h_298.15-h-0 (rec))))))

(defmethod dump+ ((sp <sp>) s)
  (labels ((rec () (first (<sp>-reccords sp))))
    (format s "~16A  ~62A~%" (<sp>-name sp) (<sp>-comments sp))
    (format s "~2D ~6A ~{~2A~6,2F~}~2D~13,7f~15,3f~%"
	    (<sp>-number-temperature-intervals sp)
	    (<sp>-reference-date-code sp) 
	    (apply #'append (<sp>-chemical-formula sp))
	    (<sp>-phase sp) 
	    (<sp>-molar-mass sp) 
	    (<sp>-heat-formation sp))
    (when (/= 0 (<sp>-number-temperature-intervals sp))
      (map nil #'(lambda (el) (dump+ el s))
	   (<sp>-reccords sp)))
    (when (= 0 (<sp>-number-temperature-intervals sp))
      (format s "~{~11,3f~}~1D~{~5,1F~}  ~15,3F~%" ;
	      (<sp-rec>-temperature-range (rec))
	      (<sp-rec>-number-coeff (rec))
	      (append-some-value-to-length
	       8 0.0 (<sp-rec>-polynomial-exponents (rec))) ;; Проверить считыватель
	      (<sp-rec>-h_298.15-h-0 (rec))))))

(defmethod dump+d->e ((sp <sp>) s)
  (labels ((rec () (first (<sp>-reccords sp))))
    (format s "~16A  ~62A~%" (<sp>-name sp) (<sp>-comments sp))
    (format s "~2D ~6A ~{~2A~6,2F~}~2D~13,7f~15,3f~%"
	    (<sp>-number-temperature-intervals sp)
	    (<sp>-reference-date-code sp) 
	    (apply #'append (<sp>-chemical-formula sp))
	    (<sp>-phase sp) 
	    (<sp>-molar-mass sp) 
	    (<sp>-heat-formation sp))
    (when (/= 0 (<sp>-number-temperature-intervals sp))
      (map nil #'(lambda (el)(dump+d->e el s))
	   (<sp>-reccords sp)))
    (when (= 0 (<sp>-number-temperature-intervals sp))
      (format s "~{~11,3f~}~1D~{~5,1F~}  ~15,3F~%" ;
	      (<sp-rec>-temperature-range (rec))
	      (<sp-rec>-number-coeff (rec))
	      (append-some-value-to-length
	       8 0.0 (<sp-rec>-polynomial-exponents (rec))) ;; Проверить считыватель
	      (<sp-rec>-h_298.15-h-0 (rec))))))

(defun lst-from-below (from below replace-nil-with lst)
  "Пример использования:
 (lst-from-below 0 5 0d0 '( 1 2 3 4 5 6 7 ))
 (lst-from-below 5 8 (make-string 16 :initial-element #\Space) '( 1 2 3 4 5 6 7 ))
"
  (substitute
   replace-nil-with nil 
   (loop :for i :from from :below below
	 :collect
	 (nth i lst))))

(defmethod dump ((rec <sp-rec>) s)
  (labels ((fmt-16-9 (lst)
	     (mapcar
	      #'(lambda (el)
		  (if (and (numberp el)(= el 0.0d0)) " 0.000000000D+00" el))
	      lst)))
    (format s "~{~11,3f~}~1D~{~5,1F~}  ~15,3F~%"
	    (<sp-rec>-temperature-range rec)
	    (<sp-rec>-number-coeff rec)
	    (append-some-value-to-length
	     8 0.0 (<sp-rec>-polynomial-exponents rec)) ;; Проверить считыватель
	    (<sp-rec>-h_298.15-h-0 rec))
    (format s "~{~16,9,2E~}~%"
	    (fmt-16-9
	     (lst-from-below 0 5
			     (make-string 16 :initial-element #\Space)
			     (<sp-rec>-coefficients rec))))
    (format s "~{~16,9,2E~}~{~16,9,2E~}~%"
	    (fmt-16-9
	     (lst-from-below 5 8
			     (make-string 16 :initial-element #\Space)
			     (<sp-rec>-coefficients rec)))
	    (fmt-16-9 (<sp-rec>-integration-constants rec)))))

(defmethod dump+ ((rec <sp-rec>) s)
  (labels ((fmt-16-9 (lst)
	     (mapcar
	      #'(lambda (el)
		  (if (and (numberp el)(= el 0.0d0)) " 0.000000000D+00" el))
	      lst)))
    (format s " ~{~10,3f~} ~1D~{~5,1F~}  ~15,3F~%"
	    (<sp-rec>-temperature-range rec)
	    (<sp-rec>-number-coeff rec)
	    (append-some-value-to-length
	     8 0.0 (<sp-rec>-polynomial-exponents rec)) ;; Проверить считыватель
	    (<sp-rec>-h_298.15-h-0 rec))
    (format s "~{~16,9,2E~}~%"
	    (fmt-16-9
	     (lst-from-below 0 5 0.0d0
			     (<sp-rec>-coefficients rec))))
    (format s "~{~16,9,2E~}~{~16,9,2E~}~%"
	    (fmt-16-9
	     (lst-from-below 5 8 0.0d0
			     (<sp-rec>-coefficients rec)))
	    (fmt-16-9 (<sp-rec>-integration-constants rec)))))

(defmethod dump+d->e ((rec <sp-rec>) s)
  (labels ((fmt-16-9 (lst)
	     (mapcar
	      #'(lambda (el)
		  (if (and (numberp el)(= el 0.0d0)) " 0.000000000E+00" el))
	      lst)))
    (format s " ~{~10,3f~} ~1D~{~5,1F~}  ~15,3F~%"
	    (<sp-rec>-temperature-range rec)
	    (<sp-rec>-number-coeff rec)
	    (append-some-value-to-length
	     8 0.0 (<sp-rec>-polynomial-exponents rec)) ;; Проверить считыватель
	    (<sp-rec>-h_298.15-h-0 rec))
    (format s "~{~16,9,2,,,,'EE~}~%"
	    (fmt-16-9
	     (lst-from-below 0 5 0.0d0
			     (<sp-rec>-coefficients rec))))
    (format s "~{~16,9,2,,,,'EE~}~{~16,9,2,,,,'EE~}~%"
	    (fmt-16-9
	     (lst-from-below 5 8 0.0d0
			     (<sp-rec>-coefficients rec)))
	    (fmt-16-9 (<sp-rec>-integration-constants rec)))))

(defmethod dump ((ht hash-table) s)
  "Сброс БД, загруженной в хештаблицу, в поток s."
  (maphash
   #'(lambda (key value)
       (declare (ignore key))
       (dump value s))
   ht))

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun substringp (needle haystack &key (test #'char=))
  "Returns the index of the first occurrence of the string designated
by NEEDLE within the string designated by HAYSTACK, or NIL if it does
not occur.  Characters within the string are compared by TEST, which
defaults to CHAR= (for case-sensitive comparison)."
  (search (string needle)
          (string haystack)
          :test test))

(defun get-db-as-string ()
  (when (null *str-db*)
    (setf *str-db*
	  (file-get-contents
	   (namestring (asdf:system-relative-pathname :gases "data/termo.inp")))))
  *str-db*)

(defmethod check-sp ((sp <sp>))
  (let* ((o-str (make-string-output-stream ))
	 (sp-str (progn
		   (dump sp o-str)
		   (get-output-stream-string o-str)))
	 (sp-str+ (progn
		    (dump+ sp o-str)
		    (get-output-stream-string o-str)))
	 (sp-str+d->e (progn
			(dump+d->e sp o-str)
			(get-output-stream-string o-str))))
    (if (or (substringp sp-str      (get-db-as-string))
	    (substringp sp-str+     (get-db-as-string))
	    (substringp sp-str+d->e (get-db-as-string)))
	t
	(progn (format t "~S~%~%~S~%~%~S~%"
		       sp-str sp-str+ sp-str+d->e)
	       nil))))

(defmethod check-sp ((<sp>-name string))
  (check-sp (get-sp <sp>-name)))

(defun check-db ()
  (let ((rezult t)
	(bad-keys nil))
    (maphash
     #'(lambda (key value)
	 (unless (check-sp value)
	   (setf rezult nil)
	   (push key bad-keys))
	 (format t "."))
     (get-db))
    (when bad-keys (format t "~&~S~%" bad-keys))
    (values rezult bad-keys)))
