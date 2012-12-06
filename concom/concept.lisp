;;;; concept.lisp
;;;; Contains the interface definition for concepts
;;;; and a nonsense example implementation for integers.

;;;; Copyright 2012 Christoph Finkensiep
;;;; This file is subject to the GUN General Public License version 3.
;;;; If a copy of the GNU GPLv3 was not distributed with this file,
;;;; You can obtain one at <http://www.gnu.org/licenses/>

(in-package #:concom)

;;; interface definition

(defgeneric related-concepts (concept brain)
  (:documentation "=> an alist of the form `(concept . weight)`"))

(defgeneric think-about (concept brain)
  (:documentation "=> a new concept derived from `concept` and `brain`
The new concept might already exist.
Additional information from `brain` may be used but the new concept should be somehow related to `concept`."))

(defgeneric update-concept (concept updates)
  (:documentation "=> `concept` with `updates` applied"))

;;; random integer implementation
;;; An integer is seen as a concept. Its neighbours are seen as related.

(defmethod related-concepts ((concept integer) brain)
  (list (cons (+ concept 1) 0.5)
	(cons (- concept 1) 0.5)))

(defmethod think-about ((concept integer) brain)
  (+ concept (if (= 1 (random 2)) 1 -1)))

(defmethod update-concept ((concept integer) (updates integer))
  concept)

;; for a bit more randomness:
(setf *random-state* (make-random-state t))
