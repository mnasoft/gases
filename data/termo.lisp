;;;; gases.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

Ar                Ref-Elm. Moore,1971. Gordon,1999.                             
 3 g 3/98 AR  1.00    0.00    0.00    0.00    0.00 0   39.9480000          0.000
    200.000   1000.0007 -2.0 -1.0  0.0  1.0  2.0  3.0  4.0  0.0         6197.428
 0.000000000D+00 0.000000000D+00 2.500000000D+00 0.000000000D+00 0.000000000D+00
 0.000000000D+00 0.000000000D+00                -7.453750000D+02 4.379674910D+00
   1000.000   6000.0007 -2.0 -1.0  0.0  1.0  2.0  3.0  4.0  0.0         6197.428
 2.010538475D+01-5.992661070D-02 2.500069401D+00-3.992141160D-08 1.205272140D-11
-1.819015576D-15 1.078576636D-19                -7.449939610D+02 4.379180110D+00
   6000.000  20000.0007 -2.0 -1.0  0.0  1.0  2.0  3.0  4.0  0.0         6197.428
-9.951265080D+08 6.458887260D+05-1.675894697D+02 2.319933363D-02-1.721080911D-06
 6.531938460D-11-9.740147729D-16                -5.078300340D+06 1.465298484D+03
C                 Hf:Douglas,1955. Moore,1970b. Gordon,1999.                    
 3 g 7/97 C   1.00    0.00    0.00    0.00    0.00 0   12.0107000     716680.000
    200.000   1000.0007 -2.0 -1.0  0.0  1.0  2.0  3.0  4.0  0.0         6535.895
 6.495031470D+02-9.649010860D-01 2.504675479D+00-1.281448025D-05 1.980133654D-08
-1.606144025D-11 5.314483411D-15                 8.545763110D+04 4.747924288D+00
   1000.000   6000.0007 -2.0 -1.0  0.0  1.0  2.0  3.0  4.0  0.0         6535.895
-1.289136472D+05 1.719528572D+02 2.646044387D+00-3.353068950D-04 1.742092740D-07
-2.902817829D-11 1.642182385D-15                 8.410597850D+04 4.130047418D+00
   6000.000  20000.0007 -2.0 -1.0  0.0  1.0  2.0  3.0  4.0  0.0         6535.895
 4.432528010D+08-2.886018412D+05 7.737108320D+01-9.715281890D-03 6.649595330D-07
-2.230078776D-11 2.899388702D-16                 2.355273444D+06-6.405123160D+02
OCCN              Cyanooxomethyl radical. Dorofeeva,2001.                       
 2 srd 01 C   2.00N   1.00O   1.00    0.00    0.00 0   54.0275000     210000.000
    200.000  1000.000 7 -2.0 -1.0  0.0  1.0  2.0  3.0  4.0  0.0        13594.351
 2.428801128D+04-5.525864490D+02 8.587838090D+00-3.379387570D-03 1.119841795D-05
-1.008408053D-08 3.086448751D-12 0.000000000D+00 2.599620066D+04-1.659592428D+01
   1000.000  6000.000 7 -2.0 -1.0  0.0  1.0  2.0  3.0  4.0  0.0        13594.351
 9.359095680D+05-4.441073340D+03 1.368958451D+01-1.647526929D-03 3.819863520D-07
-3.945148580D-11 1.509592679D-15 0.000000000D+00 4.951216910D+04-5.417083590D+01


("C                 Hf:Douglas,1955. Moore,1970b. Gordon,1999.                    "
 " 3 g 7/97 C   1.00    0.00    0.00    0.00    0.00 0   12.0107000     716680.000"
 "    200.000   1000.0007 -2.0 -1.0  0.0  1.0  2.0  3.0  4.0  0.0         6535.895"
 " 6.495031470D+02-9.649010860D-01 2.504675479D+00-1.281448025D-05 1.980133654D-08"
 "-1.606144025D-11 5.314483411D-15                 8.545763110D+04 4.747924288D+00"
 "   1000.000   6000.0007 -2.0 -1.0  0.0  1.0  2.0  3.0  4.0  0.0         6535.895"
 "-1.289136472D+05 1.719528572D+02 2.646044387D+00-3.353068950D-04 1.742092740D-07"
 "-2.902817829D-11 1.642182385D-15                 8.410597850D+04 4.130047418D+00"
 "   6000.000  20000.0007 -2.0 -1.0  0.0  1.0  2.0  3.0  4.0  0.0         6535.895"
 " 4.432528010D+08-2.886018412D+05 7.737108320D+01-9.715281890D-03 6.649595330D-07"
 "-2.230078776D-11 2.899388702D-16                 2.355273444D+06-6.405123160D+02")

(defun open-data-file ()
  (defparameter *data-file*
    (open  "/home/namatv/quicklisp/local-projects/clisp/gases/gases/data/termo.inp" :direction :input)))

(defun close-data-file ()
  (close *data-file*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-el-header ()
  (let ((str-01 (read-line *data-file*))
	(str-02 (read-line *data-file*))
	(d-01 nil)
	(d-02 nil)
	(d-03 nil)
	(t-int nil)
	(s (make-string-output-stream))
	(rez nil)
	(n-row nil))
    (format s "(~S ~S ~A ~S ((~S ~A)(~S ~A)(~S ~A)(~S ~A)(~S ~A)) ~A ~A ~A)"
	    (string-trim " " (subseq str-01 0 18))
	    (string-trim " " (subseq str-01 18))
	    (string-trim " " (SUBSEQ STR-02 0 2))
	    (string-trim " " (SUBSEQ STR-02 2 10))
	    (string-trim " " (SUBSEQ STR-02 10 12))
	    (string-trim " " (SUBSEQ STR-02 12 18))
	    (string-trim " " (SUBSEQ STR-02 18 20))
	    (string-trim " " (SUBSEQ STR-02 20 26))
	    (string-trim " " (SUBSEQ STR-02 26 28))
	    (string-trim " " (SUBSEQ STR-02 28 34))
	    (string-trim " " (SUBSEQ STR-02 34 36))
	    (string-trim " " (SUBSEQ STR-02 36 42))
	    (string-trim " " (SUBSEQ STR-02 42 44))
	    (string-trim " " (SUBSEQ STR-02 44 50))
	    (string-trim " " (SUBSEQ STR-02 50 52))
	    (string-trim " " (SUBSEQ STR-02 52 65))
	    (string-trim " " (SUBSEQ STR-02 65 80)))
    (setf rez (read (make-string-input-stream (get-output-stream-string s)))
	  n-row (third rez))
    (dotimes (i n-row)
      (setf d-01 (read-line *data-file*)
	    d-02 (read-line *data-file*)
	    d-03 (read-line *data-file*))
      (format s "((~A ~A) (~A ~A ~A ~A ~A ~A ~A ~A ~A ~A))"
	      (string-trim " " (SUBSEQ d-01 0 11))
	      (string-trim " " (SUBSEQ d-01 11 22))
	      (string-trim " " (SUBSEQ d-01 22 23))
	      (string-trim " " (SUBSEQ d-01 23 28))
	      (string-trim " " (SUBSEQ d-01 28 33))
	      (string-trim " " (SUBSEQ d-01 33 38))
	      (string-trim " " (SUBSEQ d-01 38 43))
	      (string-trim " " (SUBSEQ d-01 43 48))
	      (string-trim " " (SUBSEQ d-01 48 53))
	      (string-trim " " (SUBSEQ d-01 53 58))
	      (string-trim " " (SUBSEQ d-01 58 63))
	      (string-trim " " (SUBSEQ d-01 63 80)))
      (push  (read (make-string-input-stream (get-output-stream-string s))) t-int)
      (format s "(~A ~A ~A ~A ~A)"
	      (string-trim " " (SUBSEQ d-02 0 16))
	      (string-trim " " (SUBSEQ d-02 16 32))
	      (string-trim " " (SUBSEQ d-02 32 48))
	      (string-trim " " (SUBSEQ d-02 48 64))
	      (string-trim " " (SUBSEQ d-02 64 80)))
      (push  (read (make-string-input-stream (get-output-stream-string s))) t-int)
            (format s "(~A ~A   ~A ~A)"
	      (string-trim " " (SUBSEQ d-03 0 16))
	      (string-trim " " (SUBSEQ d-03 16 32))
;;;;	      (string-trim " " (SUBSEQ d-03 32 48))
	      (string-trim " " (SUBSEQ d-03 48 64))
	      (string-trim " " (SUBSEQ d-03 64 80)))
      (push  (read (make-string-input-stream (get-output-stream-string s))) t-int))
    (list str-01 str-02 rez n-row t-int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (close-data-file)
  (open-data-file)
  (read-line *data-file*)
  (read-line *data-file*)
  (read-el-header))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun foo (lst)
  (let ((st 0))
    (mapcar #'(lambda(el)
	       (let ((rez (list 'subseq 'str-02 st (+ el st))))
		 (setf st (+ st el)) rez))
	    lst)))

(foo (list 2 8
	   2 6 2 6
	   2 6 2 6
	   2 6
	   2 13 15))

(foo (list 11 11 1  5 5 5 5  5 5 5 5 17))

(foo (list 16 16 16 16 16))
	   16 16 16 16 16 
	  



(defun read-el-data ()
  (let ((str-01 (read-line *data-file*))
	(str-02 (read-line *data-file*))
	(str-03 (read-line *data-file*))
	(str-04 (read-line *data-file*))
	(str-05 (read-line *data-file*))
	(str-06 (read-line *data-file*))
	(str-07 (read-line *data-file*))
	(str-08 (read-line *data-file*))
	(str-09 (read-line *data-file*))
	(str-10 (read-line *data-file*))
	(str-11 (read-line *data-file*)))
    (list str-01
	  str-02
	  str-03
	  str-04
	  str-05
	  str-06
	  str-07
	  str-08
	  str-09
	  str-10
	  str-11)
))

(read-el-data)


(with-open-file (str "/home/namatv/quicklisp/local-projects/clisp/gases/gases/data/termo.inp" :direction :input)
  (string-right-trim " " )


(progn
  (close-data-file)
  (open-data-file)
  (read-line *data-file*)
  (read-line *data-file*)
  (read-el-header))

(read "0.0")

(defparameter *o* (make-string-output-stream))

(format *o* "~A" 12)

(get-output-stream-string *o*)

(defparameter *i* (make-string-input-stream "(\"as\" 12 ) "))

(read *i*)
