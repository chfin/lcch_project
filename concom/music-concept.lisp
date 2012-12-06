;;;; music-concept.lisp
;;;; An implementation of concepts in terms of notes.

;;;; Copyright 2012 Christoph Finkensiep
;;;; This file is subject to the GUN General Public License version 3.
;;;; If a copy of the GNU GPLv3 was not distributed with this file,
;;;; You can obtain one at <http://www.gnu.org/licenses/>

(in-package #:concom)

;;; special variables

(defvar *music-concept-relation-weight* 0.5
  "Defines the weight of a concepts relation to its
generalizations and instanciations.")

(defvar *thinking-direction-alpha* 0.3
  "The probability to generalize instead of specialize.")

;;; class definition

(defclass music-concept ()
  ((structure :reader music-concept-structure)
   generalizations
   instanciations))

;;; interface implementation

(defmethod related-concepts ((concept music-concept) brain)
  (with-slots (generalizations instanciations) concept
    (mapcar (lambda (c)
	      (cons c *music-concept-relation-weight*))
	    (append (generalizations instanciations)))))

;; This is a dumb prototype implementation!
(defmethod think-about ((concept music-concept) (brain cool-brain))
  (if (and (contains-vars concept)
	   (< (random 1.0 (cool-brain-random-state brain))
	      *thinking-direction-alpha*))
      (generalize concept brain)
      (specialize concept brain)))

;;; Additional implementation

(defun contains-vars (concept)
  "=> `t` if `concept` contains variables, `nil` if it is an basic musical expression"
  (not (every (lambda (subt) (typep subt note))
	      (music-concept-structure concept))))

(defun generalize (concept brain)
  "=> a more general concept, derived from `concept`"
  concept) ;;TODO: implement generalize

(defun specialize (concept brain)
  "=> an instanciation of `concept`"
  concept) ;;TODO: implement specialize

;;TODO implement functions used in think-about

;(defun variables )
