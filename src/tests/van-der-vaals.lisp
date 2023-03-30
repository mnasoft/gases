(gases/reac:q-work-low
 (gases/core:make-instance-composition
  '(( "NH3"	          1)
    )))

(gases/reac:relativ-air-mass-for-burning
 (gases/core:make-instance-composition
  '(( "NH3"	          1))))

(gases/core:density 
 (gases/core:make-instance-composition
  '(( "NH3"	          1)))
 101325.0 273.15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :half-div)

(defparameter t-kr 405.55)

(defparameter p-kr 11.32d6)

(defparameter +Rμ+ 8.314)

(defparameter a (/ (* 27/64 +Rμ+ +Rμ+ t-kr t-kr) p-kr))

(defparameter b (/ (* 1/8 +Rμ+ t-kr) p-kr))
a  ; => 0.42368798586572437d0 (42.36879858657244d0%)
b  ; => 3.723214085447493d-5 (0.003723214085447493d0%)
(defun den-NH3 (Vm aa bb PP TT)
  (-
   (*
   (+ PP (/ aa Vm Vm))
   (- Vm bb))
   (* +Rμ+ TT)))

(/ 0.01703
(half-div:h-div-lst 0.00001 100000.0 #'den-NH3 0 (list t a b 101325.0 500.0) :iters 50))
                                        ; => 0.022250054, T, 7.264316e-7, 1.0222501e-6

(/ 0.01703 0.022250054)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter b (* 1.0 72/2 1/100 1/100 1/100))
