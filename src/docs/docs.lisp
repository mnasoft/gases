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

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string= :key #'first)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  (let* ((sys-symbol :gases)
         (sys-string (string-downcase (format nil "~a" sys-symbol))))
    (mnas-package:make-html-path sys-symbol)
    (make-document)
    (mnas-package:make-mainfest-lisp `(,sys-symbol)
                                     (string-capitalize sys-string)
                                     '("Mykola Matvyeyev")
                                     (mnas-package:find-sources sys-symbol)
                                     :output-format of)
    (codex:document sys-symbol)
    (make-graphs)
    (mnas-package:copy-doc->public-html sys-string)
    (mnas-package:rsync-doc sys-string)
    :make-all-finish))
;;;; (make-all)
