(defpackage #:gases/docs
  (:use #:cl ) 
  (:nicknames "GASES/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(ases/docs) содержит функции
  генерирования и публикации документации."))

(in-package :gases/docs)

(defun make-document ()
  (loop
    :for i :in
    '((:gases                :gases)
      (:gases/molecule       nil)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for i :in
    '(:gases
      :gases/molecule
      )
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all ()
  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/gases.
"
  (make-document)
  (make-graphs)
  (mnas-package::make-mainfest-lisp
   '(:gases :gases/docs)
   "Gases"
   '("Nick Matvyeyev")
   (mnas-package::find-sources "gases"))
  (codex:document :gases)
  (make-graphs))

;;;; (make-all)
