;;;; package.lisp

;;;; Copyright 2012 Christoph Finkensiep
;;;; This file is subject to the GUN General Public License version 3.
;;;; If a copy of the GNU GPLv3 was not distributed with this file,
;;;; You can obtain one at <http://www.gnu.org/licenses/>

(defpackage #:concom
  (:use #:cl #:alexandria)
  (:export #:cool-brain
	   #:improvise #:stop-impro
	   #:start-thinking #:stop-thinking
	   
	   #:make-example-brain
	   #:start-example-thinking #:stop-example-thinking
	   #:start-example-impro))
