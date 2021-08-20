;;;; test.lisp

(in-package #:gases/gas-dynamics )

(defconstant +F-GT+ 0.004780 "м2")

;;;;                       1      2      3      4      5      6      7      8      9     10     11     12     13     14 
(defconstant +t02+     '(292.8  293.7  294.6  404.9  407.9  559.0  566.0  667.3  663.4  664.5  666.5  671.9  670.0  681.6 ))
(defconstant *p02.МТР* '(129.16 113.52 106.42 110.80 121.48 128.51 112.45 115.23 108.05 110.50 122.91 127.29 128.53 142.26))
(defconstant +G2+      '(0.5867 0.4017 0.2605 0.3017 0.3342 0.3120 0.2743 0.2823 0.1953 0.2296 0.2502 0.2663 0.2598 0.3384))

(defconstant +p02.МТР+ (mapcar #'(lambda (el) (* 1000.0 el)) *p02.МТР*))

(mapcar #'(lambda (g te p) (q-by-mfr-t-p-a-k-mu g te p +F-GT+ 1.4 0.02895)) +G2+ +t02+ +p02.МТР+ )

(defparameter *q-l* '(0.4024447 0.31398872 0.21753718 0.2836887 0.28768098 0.2972051 0.3004742  0.32767135 0.24104485 0.27732542 0.27210265 0.28077716 0.27089724 0.3215474))

(mapcar #'(lambda (tt) (a*-by-t-k-mu tt 1.4 0.02895)) +t02+)
(313.21292 313.6939 314.1742 368.3223 369.6843 432.77286 435.47412 472.8405 471.45676 471.84744 472.557 474.46747 473.79614 477.88007)

(mapcar #'(lambda (q)
	    (lambda-by-q q 1.4))
	*q-l*)

(defparameter *l-otv* '(0.26260614 0.20249128 0.13902235 0.18235731 0.18499899 0.19131422 0.19348574 0.21165133 0.15433455 0.17815351 0.1747098 0.1804328 0.17391539 0.20754576))

(mapcar #'(lambda (p tt lam)
	    (ro-by-p-t-lambda-k-mu p tt lam 1.4 0.02895))
	+p02.МТР+ +t02+ *l-otv*)

(1.4922588 1.323006 1.2477454 0.9397152 1.0223008 0.7883519 0.6810608 0.5901283  0.5615249 0.5714092 0.63399833 0.6507601 0.6596007 0.7137875)

  