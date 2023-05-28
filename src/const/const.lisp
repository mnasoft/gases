;;;; ./gases/src/const/const.lisp

(defpackage :gases/const
  (:use cl)
  (:export +Rμ+
           +kal+
           +C-0+
           +t-normal+
           +t-standard+
           +P-normal+
           +P-standard+))

(in-package :gases/const)

(defconstant +Rμ+ 8.31446261815324d0
  "Унинверсальная газовая постоянная [Дж/(моль*К)]")

(defconstant +kal+ 4.1868
  "Численное значение международной калории в Джоулях, Дж (J)")

(defconstant +C-0+ 273.15
  "Ноль шкалы Цельсия в Кельвинах, К (K)")

(defconstant +t-normal+ 273.15
  "Нормальная температура, К (K)")

(defconstant +t-standard+ 298.15
  "Стандартная температура, К (K)")

(defconstant +P-normal+ 101325.0d0
  "Нормальное атмоферное давление в Паскалях, Па")

(defconstant +P-standard+ 100000.0d0
    "Стандартное давление в Паскалях, Па")
