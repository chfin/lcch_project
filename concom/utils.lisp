;;;; utils.lisp
;;;; Some functions used in several places or just for user convenience.

;;;; Copyright 2012 Christoph Finkensiep
;;;; This file is subject to the GUN General Public License version 3.
;;;; If a copy of the GNU GPLv3 was not distributed with this file,
;;;; You can obtain one at <http://www.gnu.org/licenses/>

(in-package #:concom)

(defun draw-weighted (set weights &optional (random-state *random-state*))
  "=> a random item from `set`
`set` is an list and `weight` list of numbers which has the same length.
For each item `i` in `set`, the corresponding value in `weights` denotes `i`'s weight `w[i]`.
`i` is selected randomly with probability `w[i]` / sum_j (`w[j]`)."
  (labels ((sumup (lst acc)
	     (if lst
		 (if acc
		     (sumup (cdr lst) (cons (+ (car lst) (car acc)) acc))
		     (sumup (cdr lst) (cons (car lst) nil)))
		 acc))
	   (find-interval (lst n)
	     (if (or (null lst) (> n (car lst)))
		 (length lst)
		 (find-interval (cdr lst) n))))
    (let* ((sums (sumup weights nil))
	   (n (find-interval sums (random (car sums) random-state))))
      (nth n set))))

(defun make-concept (structure)
  (make-instance 'music-concept :structure structure))

(defun make-example-brain* ()
  "=> an instance of `cool-brain`, initialized with some integers"
  (let ((brain (make-instance 'cool-brain)))
    (with-slots (mind memory) brain
      (setf memory (alist-hash-table '((1 . 0.5) (2 . 0.3) (3 . 0.3) (4 . 0.7))))
      (setf mind (list 1 2)))
    brain))

(defvar *example-brain-ideas*
  (list (list (note :c 4)
	      (note :d 4)
	      (note :e 4)
	      (note :g 4)
	      (note :a 4)
	      (note :c* 4))
	(list (note :c* 4)
	      (note :d* 4)
	      (note :e* 4)
	      (note :g* 4)
	      (note :a* 4)
	      (note :c** 4))
	(list (var-note 'x 4)
	      (var-note 'x 8)
	      (var-note 'x 8)
	      (var-note 'x 2))
	(list 'x 'x)))

(defun make-example-brain ()
  "=> an instance of `cool-brain` with some musical expressions"
  (let* ((concepts (mapcar #'make-concept *example-brain-ideas*))
	 (mem (mapcar (lambda (c) (cons c 1)) concepts)))
    (make-instance 'cool-brain
		   :memory (alist-hash-table mem)
		   :mind concepts)))

(defvar *example-concept*
  (make-concept (list 'x 'x (var-note 'p 'd1) (var-note 'p 'd2))))
