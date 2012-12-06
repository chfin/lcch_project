;;;; package.lisp

;;;; Copyright 2012 Christoph Finkensiep
;;;; This file is subject to the GUN General Public License version 3.
;;;; If a copy of the GNU GPLv3 was not distributed with this file,
;;;; You can obtain one at <http://www.gnu.org/licenses/>

(defpackage #:concom
  (:use #:cl #:alexandria)
  (:export #:cool-brain #:make-example-brain
	   #:start-thinking #:stop-thinking))

