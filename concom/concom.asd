;;;; concom.asd

(asdf:defsystem #:concom
  :serial t
  :description "Describe concom here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:bordeaux-threads)
  :components ((:file "package")
	       (:file "notes")
	       (:file "brain")
	       (:file "output")
               (:file "concom")))
