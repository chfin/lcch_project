;;;; repl-helpers.lisp
;;;; For repl-user convenience.

;;;; Copyright 2012 Christoph Finkensiep
;;;; This file is subject to the GUN General Public License version 3.
;;;; If a copy of the GNU GPLv3 was not distributed with this file,
;;;; You can obtain one at <http://www.gnu.org/licenses/>

(in-package #:concom)

(defun make-concept (structure)
  (make-instance 'music-concept :structure structure))

(defun make-example-brain* ()
  "=> an instance of `cool-brain`, initialized with some integers"
  (let ((brain (make-instance 'cool-brain)))
    (with-slots (mind memory) brain
      (setf memory (alist-hash-table '((1 . 0.5) (2 . 0.3) (3 . 0.3) (4 . 0.7))))
      (setf mind (list 1 2)))
    brain))

(defparameter *example-brain-ideas*
  (list (list (note :c 1)) ; concept of single note
	(list 'x 'x)       ; concept of repetition
	(list 'x 'y)       ; concept of concatenation
	(list (note :e* 4) ; example concept of a motif
	      (note :c* 4)
	      (note :d* 4)
	      (note :g 4))
	(list (var-note 'x 4) ; example concept of a rhythm
	      (var-note 'x 8)
	      (var-note 'x 8)
	      (var-note 'x 2))))

(defun make-example-brain ()
  "=> an instance of `cool-brain` with some musical expressions"
  (let* ((concepts (mapcar #'make-concept *example-brain-ideas*))
	 (mem (mapcar (lambda (c) (cons c 1)) concepts)))
    (make-instance 'cool-brain
		   :memory (alist-hash-table mem)
		   :mind concepts)))

(defparameter *example-concept*
  (make-concept (list 'x 'x (var-note 'p 'd1) (var-note 'p 'd2))))

;;; running stuff

(defun start-example-impro ()
  (let ((brain (make-example-brain)))
    (improvise brain nil)))

(let ((*cool-brain-default-interval* 1))
  (defparameter *ex-brain* (make-example-brain)))

(defun start-example-thinking ()
  (start-thinking *ex-brain* t))

(defun stop-example-thinking ()
  (stop-thinking *ex-brain*))
