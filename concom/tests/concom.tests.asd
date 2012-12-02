;;;; concom.tests.asd

(asdf:defsystem #:concom.tests
  :serial t
  :description "Tests for concom"
  :author "Christoph Finkensiep <ch.finkensiep@freenet.de>"
  :license "X11/MIT"
  :depends-on (#:concom #:alexandria)
  :components ((:file "package")
               (:file "concom.tests")))
