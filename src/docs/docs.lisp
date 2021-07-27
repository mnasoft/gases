(defpackage #:gases/docs
  (:use #:cl ) 
  (:nicknames "GASES/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(ases/docs) содержит функции
  генерирования и публикации документации."))

(in-package gases/docs)

(defun make-document ()
  "@b(Описание:) функция @b(make-document) выполняет формирование
 файлов-сценариев.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-document)
@end(code)
"
  (loop
    :for i :in
    '((:gases/const        :gases/const)
      (:gases/db           :gases/db)
      (:gases/core         :gases/core)
      (:gases/reac         :gases/reac)
      (:gases/gas-dynamics :gases/gas-dynamics)
      (:gases/molecule     :gases/molecule)
      (:gases/wet-air      :gases/wet-air)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-graphs)
@end(code)
"
  (loop
    :for i :in
    '(:gases/const
      :gases/db
      :gases/core
      :gases/reac
      :gases/gas-dynamics
      :gases/molecule
      :gases/wet-air)
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all ()
  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/gases.
"
  (make-document)
  (make-graphs)
  #+nil
  (mnas-package::make-mainfest-lisp
   '(:gases :gases/docs)
   "Gases"
   '("Nick Matvyeyev")
   (mnas-package::find-sources "gases"))
  (codex:document :gases)
  (make-graphs))

;;;; (make-all)
