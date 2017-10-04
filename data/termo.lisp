;;;; gases.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

(defun open-data-file ()
  (defparameter *data-file*
    (open  "~/quicklisp/local-projects/clisp/gases/gases/data/termo.inp" :direction :input)))

(defun close-data-file ()
  (close *data-file*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-string (str &optional (ft 's))
  (cond
    ((or (eq ft 'f) (eq ft 'e) (eq ft 'd) (eq ft 'g) (eq ft 'i))
     (if (string= "" str) 0.0 (read (make-string-input-stream str))))
    ((eq ft 's) str)
    (t "Format error!")))

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
    (dotimes (i n-row)
      (mapc #'(lambda (el) (push (read-record el (read-line is)) t-int) )
       (list rec-3 rec-4 rec-5)))
    (list r-1 r-2 (reverse t-int))))

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
    (dotimes (i n-row)
      (push 
       (append (read-record rec-3 (read-line is))
	       (read-record rec-4 (read-line is))
	       (read-record rec-5 (read-line is)))
       t-int))
    (append r-1 r-2 (list (reverse t-int)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clean-termo-inp ()
  "Выполняет очистку формата ввода данных от комментариев"
  (with-open-file (os "~/quicklisp/local-projects/clisp/gases/gases/data/termo.inp.clean" :direction :output :if-exists :supersede)
    (with-open-file (is "~/quicklisp/local-projects/clisp/gases/gases/data/termo.inp" :direction :input)
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

(clean-termo-inp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-element-table()
  (let ((rez nil)
	(len 0)
	(rez-lst nil)
	)
    (with-open-file (is "~/quicklisp/local-projects/clisp/gases/gases/data/termo.inp.clean" :direction :input)
      (read-line is nil 'eof)
      (read-line is nil 'eof)
      (do ((line (read-line is nil 'eof) (read-line is nil 'eof))
	   (str-format "~A"))
	  ((eql line 'eof))
	(setf rez (read-el-header is line))
	(incf len)
	(push rez rez-lst)
;;;;	(break "rez =~S" rez)
	))
    (list len rez-lst)))

(make-element-table)


(defclass mol ()
  ((mol-name-ru       :accessor mol-name-ru       :initarg :mol-name-ru       :initform "" :documentation "Обозначение русскоязычное")
   (mol-name-en       :accessor mol-name-en       :initarg :mol-name-en       :initform "" :documentation "Обозначение англоязычное")
   
   (mol-formula       :accessor mol-formula       :initarg :mol-formula       :initform "" :documentation "Species name/formula (cols 1-15, 15str). This serves as an ID. Note that 'l' is represented by L and condensed phases designated as α, β, γ or δ are renamed a, b, c or d due to ASCII limitations.")
   (mol-comments      :accessor mol-comments      :initarg :mol-comments      :initform "" :documentation "Comments (cols 16-80, 65str). These include references in the format of author, year or page and date in the case of TRC tables. When heat of formation is taken from a separate reference, this is included as Hf:<ref>. Reference elements or species used for heat of formation calculations are indicated by Ref-Elm or Ref-Species.")
   (mol-t-int-number  :accessor mol-t-int-number  :initarg :mol-t-int-number  :initform "" :documentation "Number of temperature intervals (col 2, 2int).")
   (mol-code          :accessor mol-code          :initarg :mol-code          :initform "" :documentation "Reference-Date code (cols 4-9, 6str). This includes a character indicating a general reference followed by a date (e.g. g indicates that NASA Glenn was the source of significant work in deriving the data and 10/96 indicates the month/year).")
   (mol-atoms         :accessor mol-atoms         :initarg :mol-atoms         :initform "" :documentation "Chemical formula (cols 11-50, 2str + 6.2f). This is a set of 5 element/atom, number pairs. In the vast majority of cases the numbers are integers but in some cases they are non-integer, so floats are used.")
   (mol-phase         :accessor mol-phase         :initarg :mol-phase         :initform "" :documentation "Phase (col 52, int). Zero for gas, nonzero for condensed phases.")
   (mol-molar-mass    :accessor mol-molar-mass    :initarg :mol-molar-mass    :initform "" :documentation "Molar mass (cols 53-65, 13.5f). Originally labelled molecular weight (in units g/mol).")

   (mol-heat-formation   :accessor mol-heat-formation  :initarg :mol-heat-formation  :initform "" :documentation "Heat of formation (cols 66-80, 13.5f). In the case of condensed species this is actually an assigned enthalpy (equivalent to the heat of formation at 298.15 K). Units J/mol.")

Record 3
•	Temperature range (cols 2-21, 2x 10.3f). The minimum and maximum bounds for the current temperature interval. Units, K.
•	Number of coefficients (col 23, int). This is always 7 in this data (though the database format supports 8, see section Redundancy).
•	Polynomial exponents (cols 24-63, 8x 5.1f). These are always [-2, -1, 0, 1, 2, 3, 4] in this data.
•	{H(298.15) - H(0)} (cols 66-80, 15.3f). This is the difference between the heat of formation at the enthalpy at T = 0 K.
Record 4
•	Coefficients 1-5 (cols 1-80, 5x 16.8f).
Record 5
•	Coefficients 6-8 (cols 1-48, 3x 16.8f). The 8th is not used in this data (see section Redundancy).
•	Integration constants (cols 49-80, 2x 16.8f). Used in evaluation of enthalpy and temperature-dependent component of entropy, respectively.


   
   (mol-smile         :accessor mol-name-en       :initarg :mol-name-en       :initform "" :documentation "Smile")
   (mol-mass          :accessor mol-mass          :initarg :mol-mass          :initform "" :documentation "Молекулярная масса кг/моль")
   (mol-μcp-a-b-c     :accessor mol-μcp-a-b-c     :initarg :mol-μcp-a-b-c     :initform "" :documentation "Коэффициенты для расчета мольной теплоемкости ккал/(моль*К)")
   (mol-formula       :accessor mol-formula       :initarg :mol-formula       :initform "" :documentation "Химическая формула")
   (mol-note          :accessor mol-note          :initarg :mol-note          :initform "" :documentation "Примечание")
   )
  (:documentation "Представляет молекулу."))


