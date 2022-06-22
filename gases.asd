;;;; gases.asd

(defsystem "gases"
  :version "0.2.0"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("gases/const"
               "gases/db"
               "gases/core"
               "gases/reac"
               "gases/molecule"
               "gases/gas-dynamics"
               "gases/wet-air"
               #+nil "gases/web" )
  :description "Система содержит функции, предназначенные для
  определения термодинамических свойств
@begin(list)
 @item(- простых веществ; )
 @item(- газовых смесей; )
 @item(- влажного воздуха. )
@end(list)
"
  )

(defsystem "gases/core"
  :version "0.2.0"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("cl-utilities" "half-div" "math" "gases/const" "gases/db") 
  :components ((:module "src/core"
		:serial t
		:components
		((:file "core")
                 ;; (:file "example-gas")
		 )))
  :description "Система содержит функции, предназначенные для
  определения термодинамических свойств простых веществ и газовых
  смесей."
  :in-order-to ((test-op (test-op "gases/tests"))))

(defsystem "gases/reac"
  :version "0.2.0"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("cl-utilities" "half-div" "math" "gases/const" "gases/db" "gases/core") 
  :components ((:module "src/reac"
		:serial t
		:components
		((:file "reac")

		 )))
  :description "Система содержит функции, предназначенные для
  определения описания реакций."
  :in-order-to ((test-op (test-op "gases/tests"))))

(defsystem "gases/const"
  :version "0.2.0"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
;;  :depends-on () 
  :components ((:module "src/const"
		:serial t
		:components
		((:file "const")
                 ;; (:file "example-gas")
		 )))
  :description "Определяет некторые физические константы."
  :in-order-to ((test-op (test-op "gases/tests"))))

(defsystem "gases/db"
  :version "0.2.0"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("gases/const") 
  :components ((:module "src/db"
		:serial t
		:components
		((:file "db"))))
  :description "Система определяет интерфейс к базе данных
  термодинамических свойств простых веществ."
  :in-order-to ((test-op (test-op "gases/tests"))))

(defsystem "gases/tests"
  :depends-on (:gases :math :fiveam)
  :perform (test-op (o s)
		    (uiop:symbol-call :gases-tests :test-gases))
  :components ((:module "src/tests"
		:serial t
		:components ((:file "main")))))

(defsystem "gases/molecule"
  :version "0.2.0"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
;;  :depends-on ("gases") 
  :components ((:module "src/molecule"
		:serial t
		:components ((:file "molecule"))))
  :description "Система содержит функции для определения тепловых
  свойств вещества." )

(defsystem "gases/wet-air"
  :version "0.2.0"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  ;;  :depends-on () ;; "gases" #:cl-annot
  :components ((:module "src/wet-air"
		:serial t
		:components ((:file "wet-air"))))
  :description
  "@b(Описание:) система @b(gases/wet-air) содержит функции для
 определения термодинамических свойств влажного воздуха: 

@begin(list)
 @item(влагосодержание влажного воздуха;) 
 @item(давление насыщения водяных паров;) 
 @item(температура точки росы;) 
 @item(энтальпия влажного воздуха.) 
@end(list)")

(defsystem "gases/gas-dynamics"
  :version "0.1.0"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"  
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("half-div" "gases/const") ;; "gases"
  :components ((:module "src/gas-dynamics"
		:components ((:file "main"))))
  :description "Система содержит газодинамические функции."
  :in-order-to ((test-op (test-op "gases/tests"))))

(defsystem "gases/web"
  :version "0.0.2"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("math" "periodic-table" "caveman" "cl-who") ;; "gases" "cl-annot"
  :components ((:module "src/web"
		:serial t
		:components
		((:file "web"))))
  :description "@b(Описание:) система @b(gases/web) определяет
  WEB-интерфейс для выбора простых веществ из базы данных."
;;  :in-order-to ((test-op (test-op "gases/tests")))
  )

(defsystem "gases/docs"
  :description "@b(Описание:) система @b(gases/docs) содержит
  зависимости для сборки документации."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("gases" "mnas-package" "codex")
  :components ((:module "src/docs"
		:serial nil
                :components ((:file "docs")))))

;;;;(:module "gas-dynamics" :depends-on (#:half-div) :components ((:file "src/main")))

;;;;/home/namatv/quicklisp/local-projects/clisp/gases/gases/src/main.lisp:
