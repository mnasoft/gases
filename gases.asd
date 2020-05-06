;;;; gases.asd

(defsystem #:gases
  :version "0.1.0"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on (#:mnas-defclass #:half-div)
  :components
			((:file "package")
			 (:file "gases")
			 (:file "classes")
			 (:file "defmethods")
			 (:file "termo")
			 (:file "air")
			 ;; (:file "elements")
			 ;; (:file "running-gas")
			 ;; (:file "stopping-gas")			 
			 )

;  :components ((:module "src"
;			:components
;			((:file "package")
;			 (:file "gases")
;			 (:file "classes")
;			 (:file "defmethods")
;			 (:file "termo")
;			 ;; (:file "elements")
;			 ;; (:file "running-gas")
;			 ;; (:file "stopping-gas")
;			 (:file "air"))))
;  :description "Проект содержит некоторые формулы термодинамики"
;  :in-order-to ((test-op (test-op "gases/tests")))
  )

;(defsystem #:gases/gas-dynamics
;  :version "0.1.0"
;  :author "Nick Matvyeyev <mnasoft@gmail.com>"  
;  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
;  :depends-on ("gases" "half-div")
;  :components ((:module
;		"gas-dynamics/src"
;		:components
;		((:file "main")
;		 )))
;  :description "Проект содержит некоторые газодинамические функции."
;  :in-order-to ((test-op (test-op "gases/tests"))))

;;;;(:module "gas-dynamics" :depends-on (#:half-div) :components ((:file "src/main")))

;;;;/home/namatv/quicklisp/local-projects/clisp/gases/gases/src/main.lisp:
