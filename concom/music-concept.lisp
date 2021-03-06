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

(defvar *thinking-direction-alpha* 0.7
  "The probability to specialize instead of generalize.")

;;; class definition

(defclass music-concept ()
  ((structure :reader music-concept-structure
	      :initarg :structure
	      :initform (error "No :structure provided"))
   (derivates :accessor music-concept-derivates
	      :initform nil)
   (source :reader music-concept-source
	   :initarg :source
	   :initform nil)
   (generalizations :accessor music-concept-gens
		    :initform nil)
   (instanciations :accessor music-concept-insts
		   :initform nil)))

(defmethod print-object ((object music-concept) stream)
  (format stream "[~{~a~^ ~}]" (music-concept-structure object)))

(defmethod equals ((a music-concept) (b music-concept))
  (let ((s1 (music-concept-structure a))
	(s2 (music-concept-structure b)))
    (and (= (length s1) (length s2))
	 (every #'equals s1 s2))))

;;; interface implementation

(defmethod related-concepts ((concept music-concept) brain)
  (with-slots (generalizations instanciations) concept
    (mapcar (lambda (c)
	      (cons c *music-concept-relation-weight*))
	    (append generalizations instanciations))))

(defmethod think-about ((concept music-concept) (brain cool-brain))
  (if (or (and (contains-vars concept)
	       (< (random 1.0 (cool-brain-random-state brain))
		  *thinking-direction-alpha*))
	  (null (atomar-vals concept)))
      (specialize concept brain)
      (generalize concept brain)))

(defmethod update-concept ((concept music-concept) (updates music-concept))
  (with-slots (derivates generalizations instanciations) concept
    (setf derivates (music-concept-derivates updates))
    (setf generalizations (music-concept-gens updates))
    (setf instanciations (music-concept-insts updates)))
  concept)

;;; Note variables

(defclass var-note ()
  ((pitch :initarg :p
	  :initform (error "pitch not provided")
	  :reader var-note-pitch)
   (duration :initarg :d
	     :initform (error "duration not provided")
	     :reader var-note-dur)))

(defmethod print-object ((object var-note) stream)
  (with-slots (pitch duration) object
    (format stream "<~a,~a>"
	  (if (typep pitch 'pitch)
	      (gethash (var-note-pitch object) *pitch-name-table* "-")
	      pitch)
	  duration)))

(defmethod equals ((a var-note) (b var-note))
  (and (equals (var-note-pitch a) (var-note-pitch b))
       (equals (var-note-dur a) (var-note-dur b))))

(defun var-note (pitch duration)
  "=> an instance of `var-note` with `pitch` as pitch and `duration` as duration"
  (make-instance 'var-note :p pitch :d duration))

;;; generalization

(defun generalize (concept brain)
  "=> a more general concept, derived from `concept`"
  (declare (ignore brain))
  (when *debug-cool-brain* (format t "~%Generalizing!"))
  (let* ((atoms (atomar-vals concept))
	 (atom (random-elt atoms))
	 (result (varify concept (car atom) (gensym (symbol-name (cdr atom))))))
    (push concept (music-concept-insts result))
    (push result (music-concept-gens concept))
    result))

;;; specialization

(defun specialize (concept brain)
  "=> an instanciation of `concept`"
  (when *debug-cool-brain* (format t "~%Specializing!")) 
  (let* ((vars (music-variables concept))
	 (bindings (mapcar (music-binder brain) vars))
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
	       (let* ((candidates (remove-if #'contains-vars mind))
		      (terms (or candidates (some-music brain)))
		      (weights (mapcar (lambda (c) (gethash c memory 0)) terms))
		      (term (draw-weighted terms weights random-state)))
		 (music-concept-structure term))))))))

;;; Additional implementation

(defun contains-vars (concept)
  "=> `t` if `concept` contains variables, `nil` if it is an basic musical expression"
  (not (every (lambda (subt) (typep subt 'note))
	      (music-concept-structure concept))))

;;; equality of musical expressions

(defgeneric equals (a b)
  (:documentation "=> t, if a and b are equal, nil otherwise"))

(defmethod equals (a b)
  (equal a b))

(defmethod equals ((a symbol) (b symbol))
  (eq a b))

(defmethod equals ((a var-note) (b var-note))
  (and (equals (var-note-pitch a) (var-note-pitch b))
       (equals (var-note-dur a) (var-note-dur b))))

;;; getting atomar expressions

(defgeneric atomar-vals (expression)
  (:documentation
   "=> an alist of values and their types from `expression`
types can be `:note`, `:pitch`, and `:duration`"))

(defmethod atomar-vals (expression)
  nil)

(defmethod atomar-vals ((exp note))
  (with-slots (pitch duration) exp
    (list (cons pitch :pitch)
	  (cons duration :duration))))

(defmethod atomar-vals ((exp var-note))
  (with-slots (pitch duration) exp
    (let ((p? (typep pitch 'pitch))
	  (d? (typep duration 'duration)))
      (append (when p? (list (cons pitch :pitch)))
	      (when d? (list (cons duration :duration)))
	      (when (not (or p? d?)) (list (cons exp :note)))))))

(defmethod atomar-vals ((exp music-concept))
  (remove-duplicates
   (mapcan #'atomar-vals (music-concept-structure exp))
   :key #'car
   :test #'equals))

;;; "varifying" a concept. Take one value and replace it with a symbol

(defgeneric varify (expression atom var))

(defmethod varify (exp atom var)
  (if (equals exp atom) var exp))

(defun varify-note (note atom var)
  (with-slots (pitch duration) note
    (let ((p? (equals pitch atom))
	  (d? (equals duration atom)))
      (if (or p? d?)
	  (var-note (if p? var pitch)
		    (if d? var duration))
	  note))))

(defmethod varify ((exp note) atom var)
  (varify-note exp atom var))

(defmethod varify ((exp var-note) atom var)
  (if (equals exp atom)
      var
      (varify-note exp atom var)))

(defmethod varify ((exp music-concept) atom var)
  (with-slots (structure) exp
    (let* ((strnew (mapcar (lambda (e) (varify e atom var)) structure))
	   (result (make-instance 'music-concept :structure strnew :source exp)))
      (push result (music-concept-derivates exp))
      result)))

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
      (when (not (typep pitch 'pitch))
	(push (cons pitch :pitch) vars))
      (when (symbolp duration)
	(push (cons duration :duration) vars))
      vars)))

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
    (let* ((strnew (flatten (mapcar (lambda (e) (subst-vars e bindings))
				    structure)))
	   (result (make-instance 'music-concept :structure strnew :source exp)))
      (push result (music-concept-derivates exp))
      result)))
