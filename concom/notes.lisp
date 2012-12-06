;;;; notes.lisp

;;;; Copyright 2012 Christoph Finkensiep
;;;; This file is subject to the GUN General Public License version 3.
;;;; If a copy of the GNU GPLv3 was not distributed with this file,
;;;; You can obtain one at <http://www.gnu.org/licenses/>

(in-package #:concom)

(defvar *pitches* '(:c :d :e :g :a :c* :d* :e* :g* :a* :c**))
(defvar *pitch-names* '("c" "d" "e" "g" "a"
			"c'" "d'" "e'" "g'" "a'"
			"c''"))
(defvar *durations* '(1 2 4 8 16 32))

(deftype pitch ()
  `(member ,@*pitches*))

(deftype duration ()
  `(member ,@*durations*))

(defclass note ()
  ((pitch :initarg :p
	  :initform (error "no pitch provided")
	  :reader note-pitch
	  :type pitch)
   (duration :initarg :d
	     :initform (error "no duration provided")
	     :reader note-dur
	     :type duration)))

(defun note (pitch dur)
  "=> an instance of class `note` with pitch `pitch` and duration `dur`"
  (make-instance 'note :p pitch :d dur))

;;; printing

(defparameter *pitch-name-table*
  (alist-hash-table (mapcar #'cons *pitches* *pitch-names*)))

;(defmethod print-object ((object pitch) stream)
;  (format stream "~a" (gethash object *pitch-name-table*)))

(defmethod print-object ((object note) stream)
  (format stream "~a~a"
	  (gethash (note-pitch object) *pitch-name-table*)
	  (note-dur object)))
