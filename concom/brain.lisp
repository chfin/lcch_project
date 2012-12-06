;;;; brain.lisp
;;;; Defines the interface to a brain,
;;;; as well as the random brain and the static brain.

;;;; Copyright 2012 Christoph Finkensiep
;;;; This file is subject to the GUN General Public License version 3.
;;;; If a copy of the GNU GPLv3 was not distributed with this file,
;;;; You can obtain one at <http://www.gnu.org/licenses/>

(in-package #:concom)

;;; brain interface

(defgeneric start-thinking (brain &optional debug)
  (:documentation "Should do the setup for the brain to start thinking (e.g. starting a thread with a loop)."))

(defgeneric stop-thinking (brain)
  (:documentation "Can be called to stop the brain from thinking (e.g. stop the loop thread)."))

(defgeneric set-topic (brain music)
  (:documentation "Sets the current *mind* of `brain` to  `music` which is a list of notes.
If `music` is nil, nothing should happen."))

(defgeneric get-next-music (brain)
  (:documentation "=> some music produced by `brain` (i.e. a list of notes)."))

;;; fallback methods

(defmethod start-thinking (brain &optional debug) nil)
(defmethod stop-thinking (brain) nil)
(defmethod set-topic (brain music) nil)
(defmethod get-next-music (brain) nil)

;;; random brain

(defclass random-brain ()
  ((random-state :reader random-brain-random-state
		 :initform (make-random-state t))))

(defmethod get-next-music ((brain random-brain))
  (labels ((r (arg)
	     (random arg (random-brain-random-state brain)))
	   (random-note ()
	     (note (nth (r (length *pitches*)) *pitches*)
		   (nth (r (length *durations*)) *durations*))))
    (cons (random-note)
	  (loop while (> (r 1.0) 0.25)
	     collect (random-note)))))
