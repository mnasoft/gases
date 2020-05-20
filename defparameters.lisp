;;;; defparameters.lisp

(in-package :gases)

(annot:enable-annot-syntax)

(defparameter *sp-db* (make-hash-table :test #'equal)
  "База данных компонентов")

(defparameter *str-db* nil
  "Строковое представление базы данных.
 Содержимое файла data/termo.inp")
