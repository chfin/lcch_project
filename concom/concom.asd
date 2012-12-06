;;;; concom.asd

;;;; Copyright 2012 Christoph Finkensiep
;;;; This file is subject to the GUN General Public License version 3.
;;;; If a copy of the GNU GPLv3 was not distributed with this file,
;;;; You can obtain one at <http://www.gnu.org/licenses/>

(asdf:defsystem #:concom
  :serial t
  :description "ConCom is a system for musical composition and improvisation."
  :author "Christoph Finkensiep <ch.finkensiep@freenet.de>"
  :license "GNU GPLv3"
  :depends-on (#:alexandria #:bordeaux-threads)
  :components ((:file "package")
	       (:file "utils")
	       (:file "notes")
	       (:file "concept")
	       (:file "brain")
	       (:file "coolbrain")
	       (:file "music-concept")
	       (:file "output")
               (:file "concom")))
