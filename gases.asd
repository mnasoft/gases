;;;; gases.asd

(defsystem #:gases
  :description "Describe gases here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on (#:mnas-defclass)
  :serial t
  :components ((:file "package")
               (:file "gases")
	       (:file "classes")
	       (:file "defmethods")
	       (:file "termo")
	       ;; (:file "elements")
	       ;; (:file "running-gas")
	       ;; (:file "stopping-gas")
	       (:file "air")
	       ))
