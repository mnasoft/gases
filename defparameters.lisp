;;;; defparameters.lisp

(in-package :gases)

(annot:enable-annot-syntax)

(defparameter *sp-db* (make-hash-table :test #'equal)
  "База данных компонентов")

(defparameter *str-db* nil
  "Строковое представление базы данных.
 Содержимое файла data/termo.inp")

@annot.doc:doc
"Список имен компонентов неучаствующих в реакции окисления кислородом O2."
(defparameter *not-combasted-sp-names* '("N2" "O2" "H2O" "CO2" "SO2" "SO3" "He" "Ar" "Kr" "Xe" "Rd"))

@annot.doc:doc
"Хеш-таблица ключ - имя; значение - объект класса <sp>,
 неучаствующих в реакции окисления кислородом O2."
(defparameter *not-combasted-sp* (make-hash-table :test #'equal))

