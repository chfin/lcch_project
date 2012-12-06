;;;; utils.lisp
;;;; Some functions used in several places.

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

(defun some-music (brain &optional (n *default-best-concepts-count*))
  "=> at most `n` terms from the memory of `brain` without variables"
  (with-slots (memory) brain
    (let ((ts (remove-if #'contains-vars (hash-table-keys memory))))
      (loop  for i from 1 to n while ts
	 collect (let ((re (random-elt ts)))
		   (setf ts (remove re ts))
		   re)))))

;;; equality of musical expressions

(defgeneric equals (a b)
  (:documentation "=> t, if a and b are equal, nil otherwise"))

(defmethod equals (a b)
  (equal a b))

(defmethod equals ((a symbol) (b symbol))
  (eq a b))
