;;;; components.lisp

(in-package #:gases)

;;; "gases" goes here. Hacks and glory await!

(defparameter *Воздух*       '( 6.557   1.477  -0.2148 0.02896 )     "Воздух            0.02896")
(defparameter *NH3*          '( 6.086   8.812  -1.506  0.0170306 )   "Аммиак NH3        (+ 14 3)")
(defparameter *Br2*          '( 8.4228  0.9739 -0.3555 0.159808)     "Бром              (* 79.904 2)")
(defparameter *CO2*          '( 6.214  10.396  -3.545  0.04401)      "Двуокись углерода (+ (* 12 1) (* 16 2))")
(defparameter *CO*           '( 6.420   1.665  -0.196  0.02801)      "Окись углерода    (+ (* 12 1) (* 16 1))")
(defparameter *Cl2*          '( 7.5755  2.4244 -0.9650 0.070903)     "Хлор              (+ 35.446 35.457) ")
(defparameter *H2*           '( 6.9469 -0.1999  0.4808 0.0020159502) "Водород           (+ 1.00784 1.00811) ")
(defparameter *HBr*          '( 5.5776  0.9549  0.1581 0.08091)      "Бромистый водород 80.91 ")
(defparameter *HCl*          '( 6.732   0.4325  0.3697 0.0364606)    "Хлористый водород 36.46061 ")
(defparameter *H2S*          '( 6.662   5.134  -0.854  0.034082)     "Хлористый водород 34.082 ")
(defparameter *N2*           '( 6.4492  1.4125 -0.0807 0.02801371)   "Азот N2 (+ 14.00643 14.00728)")
(defparameter *NO*           '( 6.440    2.069 -0.4206 0.02801371)   "Оксид азота  NO  30.0061")
(defparameter *N2O*          '( 6.529   10.515 -3.571  0.0440128)    "Закись азота N2O 44.0128")
(defparameter *O2*           '( 6.0954  3.2533 -1.0171 0.02801371)   "Кислород O2 (+ 15.99903 15.99977)")
(defparameter *PH3*          '( 4.496  14.372  -4.072  0.03400)      "Фосфин PH3  34.00")
(defparameter *SO2*          '( 7.116   9.512  -3.511  0.0640540)    "Двуокись серы SO2  64.0540")
(defparameter *SO3*          '( 6.077  23.537  -9.687  0.08006)      "Серный ангидрид SO3  80.06")
(defparameter *H2O*          '( 7.256   2.298   0.283  0.01801528)   "Вода H2O (+ (* 16 1) 2)") ; !!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *C1*           '( 3.381  18.044  -4.300  0.01604) "Метан CH4 (+ (* 12 1) 4)") ; !!!
(defparameter *C2*           '( 2.247  38.201 -11.049  0.03007) "Этан C2H6 (+ (* 12 2) 6) ")
(defparameter *C3*           '( 2.410  57.195 -17.533  0.04410) "Пропан C3H8 (+ (* 12 3) 8)")

(defparameter *nC4*          '( 4.453  72.270 -22.214  0.05812) "н-Бутан C4H10 (+ (* 12 4) 10)")
(defparameter *2MC3*         '( 3.332  75.214 -23.734  0.05812) "2-Метил-Пропан (изо-Бутан) C4H10 (+ (* 12 4) 10)")
(defparameter *iC4*          *2MC3*                             "изо-Бутан C4H10 (+ (* 12 4) 10)")

(defparameter *nC5*          '( 5.910  88.449 -27.388  0.07215) "н-Пентан C5H12 (+ (* 12 5) 12)")
(defparameter *iC5*          *nC5*                              "изо-Пентан принят по н-Пентану C5H12 (+ (* 12 5) 10)")

(defparameter *nC6*          '( 7.477 104.422 -32.471  0.08617848) "н-Гексан C6H14 (+ (* 12 6) 14)")
(defparameter *nC7*          '( 9.055 120.352 -37.528  0.10021) "н-Гептан C7H16 (+ (* 12 7) 16)")
(defparameter *nC8*          '(10.626 136.298 -42.592  0.11423) "н-Октан C8H18 (+ (* 12 8) 18)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *C2H4*          '( 2.830  28.601  -8.726  0.02805) "Этилен C2H4        28.05")
(defparameter *C3H6*          '( 3.253  45.116 -13.740  0.04208) "Пропилен C3H6      42.08")
(defparameter *1-C4H8*        '( 5.132  61.760 -19.322  0.05611) "1-Бутен C4H8       56.11")
(defparameter *cys-2-C4H8*    '( 1.625  64.836 -20.047  0.05611) "цис-2-Бутен C4H8   56.11")
(defparameter *trans-2-C4H8*  '( 4.967  59.961 -18.147  0.05611) "транс-2-Бутен C4H8 56.11")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *C2H2*          '( 7.331  12.622  -3.886  0.026038) "Ацетилен C2H4  26.038")
(defparameter *C3H4*          '( 6.334  30.990  -9.457  0.04006)  "Пропин   C3H6  40.06")
(defparameter *2-C4H6*        '( 5.700  48.207 -14.479  0.05409)  "2-Бутин  C4H8  54.09")

(defparameter *Бензол*        '(-0.409  77.621 -26.429  0.07811) "Бензол C6H6         78.11")
(defparameter *Толуол*        '( 0.576  93.593 -31.227  0.09214) "Толуол CH3-C6H6     92.14")
(defparameter *Стирол*        '( 4.074  99.731 -33.108  0.10415) "Стирол CH2=C-C6H6  104.15")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *Cyclo_C5*     '(-5.763  97.377 -31.328  0.0701)  "Циклопентан C5H10 70.1  (+ (* 12 5) 10)")
(defparameter *Cyclo_C6*     '(-7.701 125.675 -41.584  0.08416) "Циклогексан C6H12 84.16 (+ (* 12 6) 12)")
(defparameter *Mcyclo_C5*    '(-7.701 125.675 -41.584  0.08416) "метил-Цеклопентан принят по Циклогексану C6H12 84.16 (+ (* 12 6) 12)")
(defparameter *Mcyclo_C6*    '( 9.055 120.352 -37.528  0.09819) "метил-Цеклогексан принят по н-Гексану C7H14 98.19 (+ (* 12 7) 14)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
