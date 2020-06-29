;;;; gases.asd

(defsystem #:gases
  :version "0.2.0"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on (#:cl-annot #:cl-utilities #:half-div #:math ) ;; #:mnas-defclass CL-UTILITIES
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
		 (:file "air")
		 (:file "example-gas"))))
  :description "Проект содержит некоторые формулы термодинамики"
  :in-order-to ((test-op (test-op "gases/tests"))))

(defsystem #:gases/tests
  :depends-on (:gases :math :fiveam)
  :perform (test-op (o s)
		    (uiop:symbol-call :gases-tests :test-gases))
  :components ((:module "src/tests"
		:serial t
		:components ((:file "main")))))

(defsystem #:gases/molecule
  :version "0.2.0"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on (#:cl-annot #:gases)
  :components ((:module "molecule"
		:serial t
		:components ((:file "elements")))))

(defsystem #:gases/gas-dynamics
  :version "0.1.0"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"  
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("gases" "half-div")
  :components ((:module
		"gas-dynamics/src"
		:components
		((:file "main"))))
  :description "Проект содержит некоторые газодинамические функции."
  :in-order-to ((test-op (test-op "gases/tests"))))

;;;;(:module "gas-dynamics" :depends-on (#:half-div) :components ((:file "src/main")))

;;;;/home/namatv/quicklisp/local-projects/clisp/gases/gases/src/main.lisp:
