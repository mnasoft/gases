;;;; gases.asd

(asdf:defsystem #:gases
  :description "Describe gases here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
	       (:file "components")
               (:file "gases")
	       (:file "running-gas")
	       (:file "stopping-gas")))
