;;;; gases.asd

(defsystem "gases"
  :version "0.2.0"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("cl-utilities"
               "half-div"
               "math"
               
               "gases/molecule"
               "gases/gas-dynamics") 
  :components ((:module "src"
		:serial t
		:components
		((:file "package")
		 (:file "gases")
		 (:file "defparameters")
		 (:file "classes")
		 (:file "termo")
		 (:file "defgenerics")
		 (:file "defmethods")
		 (:file "burning")
                                        ;(:file "example-gas")
		 (:file "select"))))
  :description "Проект содержит некоторые формулы термодинамики"
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
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
;;  :depends-on ("gases") 
  :components ((:module "src/molecule"
		:serial t
		:components ((:file "molecule")))))

(defsystem "gases/wet-air"
  :version "0.2.0"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on (#:cl-annot #:gases)
  :components ((:module "src/wet-air"
		:serial t
		:components ((:file "air")))))

(defsystem "gases/gas-dynamics"
  :version "0.1.0"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"  
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("half-div") ;; "gases"
  :components ((:module "src/gas-dynamics"
		:components ((:file "main"))))
  :description "Проект содержит некоторые газодинамические функции."
  :in-order-to ((test-op (test-op "gases/tests"))))

(defsystem "gases/web"
  :version "0.0.1"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("gases" "math" "periodic-table" "caveman" "cl-who") ;; "cl-annot"
  :components ((:module "src/web"
		:serial t
		:components
		((:file "web"))))
  :description "Проект содержит некоторые газодинамические функции."
;;  :in-order-to ((test-op (test-op "gases/tests")))
  )

(defsystem "gases/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("gases" "mnas-package" "codex")
  :components ((:module "src/docs"
		:serial nil
                :components ((:file "docs")))))

;;;;(:module "gas-dynamics" :depends-on (#:half-div) :components ((:file "src/main")))

;;;;/home/namatv/quicklisp/local-projects/clisp/gases/gases/src/main.lisp:
