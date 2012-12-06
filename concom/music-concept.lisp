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
  ((structure :reader music-concept-structure
	      :initarg :structure
	      :initform (error "No :structure provided"))
   (generalizations :accessor music-concept-gens
		    :initform nil)
   (instanciations :accessor music-concept-insts
		   :initform nil)))

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

;;; Note variables

(defclass var-note ()
  ((pitch :initarg :p
	  :initform (error "pitch not provided")
	  :reader var-note-pitch)
   (duration :initarg :d
	     :initform (error "duration not provided")
	     :reader var-note-dur)))

(defun var-note (pitch duration)
  "=> an instance of `var-note` with `pitch` as pitch and `duration` as duration"
  (make-instance 'var-note :p pitch :d duration))

;;; generalization

(defun generalize (concept brain)
  "=> a more general concept, derived from `concept`"
  concept);;TODO: implement generalize

;;; specialization

(defun specialize (concept brain)
  "=> an instanciation of `concept`"
  (let* ((vars (music-variables concept))
	 (bindings (print (mapcar (music-binder brain) vars)))
	 (result (subst-vars concept bindings)))
    (push concept (music-concept-gens result))
    (push result (music-concept-insts concept))
    result))

;;Ok, now this is really dumb. Just take a random element.
(defun music-binder (brain)
  "=> a function, that creates a binding for a variable with some value"
  (lambda (var)
    (cons (car var)
	  (case (cdr var)
	    (:pitch (random-elt *pitches*))
	    (:duration (random-elt *durations*))
	    (:music
	     (with-slots (mind memory random-state) brain
	       (let* ((terms (remove-if #'contains-vars mind))
		      (weights (mapcar (lambda (c) (gethash c memory 0)) terms))
		      (term (draw-weighted terms weights random-state)))
		 (music-concept-structure term))))))))

;;; Additional implementation

(defun contains-vars (concept)
  "=> `t` if `concept` contains variables, `nil` if it is an basic musical expression"
  (not (every (lambda (subt) (typep subt 'note))
	      (music-concept-structure concept))))

;;; getting variables

(defgeneric music-variables (expression)
  (:documentation
   "=> the alist of variables and their types contained in `expression`
Valid types are `:music`, `:pitch`, and `:duration`."))

(defmethod music-variables (exp)
  (declare (ignore exp))
  nil)

(defmethod music-variables ((exp symbol))
  (list (cons exp :music)))

(defmethod music-variables ((exp var-note))
  (with-slots (pitch duration) exp
    (let ((vars nil))
      (when (and (symbolp pitch) (not (keywordp pitch)))
	(push (cons pitch :pitch) vars))
      (when (symbolp duration)
	(push (cons duration :duration) vars)))))

(defmethod music-variables ((exp music-concept))
  (remove-duplicates
   (mapcan #'music-variables (music-concept-structure exp))
   :key #'car))

;;; substituting variables

(defun bval (var bindings)
  (or (assoc-value bindings var)
      (error "No binding for ~a" var)))

(defgeneric subst-vars (exp bindings)
  (:documentation "=> `exp` with variables substituted according to `bindings`"))

(defmethod subst-vars (exp bindings)
  exp)

(defmethod subst-vars ((exp symbol) bindings)
  (bval exp bindings))

(defmethod subst-vars ((exp var-note) bindings)
  (with-slots (pitch duration) exp
    (let ((p (if (keywordp pitch)
		 pitch
		 (bval pitch bindings)))
	  (d (if (symbolp duration)
		 (bval duration bindings)
		 duration)))
      (note p d))))

(defmethod subst-vars ((exp music-concept) bindings)
  (with-slots (structure) exp
    (let ((strnew (mapcar (lambda (e) (subst-vars e bindings)) structure)))
      (make-instance 'music-concept :structure strnew))))
