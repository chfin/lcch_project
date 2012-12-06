;;;; coolbrain.lisp
;;;; Contains the real brain implementation.

;;;; Copyright 2012 Christoph Finkensiep
;;;; This file is subject to the GUN General Public License version 3.
;;;; If a copy of the GNU GPLv3 was not distributed with this file,
;;;; You can obtain one at <http://www.gnu.org/licenses/>

(in-package #:concom)

;;; special vars for default brain values

(defvar *cool-brain-default-interval* 1
  "Default interval between thoughts in seconds.")

(defvar *cool-brain-default-alpha* 0.5
  "Default alpha value used to update the weights (exponential moving average).")

(defvar *cool-brain-default-relation-threshold* 0.1
  "Default minimum weight of a node to further explore its relations.")

(defvar *cool-brain-default-push-threshold* 0.2
  "Default weight threshold above which concepts are pushed to the mind.")

(defvar *cool-brain-default-drop-threshold* 0.1
  "Default weight threshold below which concepts are dropped from the mind.")

(defvar *default-best-concepts-count* 10
  "If the mind is empty, this is the default number of concepts to draw from the memory.")

(defvar *debug-cool-brain* t)

;;; class definition

(defclass cool-brain ()
  ((mind :initform nil
	 :initarg :mind
	 :reader cool-brain-mind)
   (memory :initform (make-hash-table)
	   :initarg :memory)
   (thinking :initform nil
	     :accessor cool-brain-thinking-p
	     :type boolean)
   (random-state :initform (make-random-state t)
		 :type random-state
		 :reader cool-brain-random-state)
   (interval :initform *cool-brain-default-interval*
	     :type number
	     :accessor cool-brain-interval)
   (alpha :initform *cool-brain-default-alpha*
	  :initarg :alpha
	  :type number
	  :accessor cool-brain-alpha)
   (relation-threshold :initform *cool-brain-default-relation-threshold*
		       :type number
		       :accessor cool-brain-relation-threshold)
   (push-threshold :initform *cool-brain-default-push-threshold*
		   :type number)
   (drop-threshold :initform *cool-brain-default-drop-threshold*
		   :type number)))

;;; brain interface implementation

(defmethod start-thinking ((brain cool-brain))
  (setf (cool-brain-thinking-p brain) t)
  (bordeaux-threads:make-thread
   (lambda () (cool-thinking brain))))

(defmethod stop-thinking ((brain cool-brain))
  (setf (cool-brain-thinking-p brain) nil))

(defmethod set-topic ((brain cool-brain) music)
  (error "set-topic not implemented for cool-brain"))

(defmethod get-next-music ((brain cool-brain))
  (error "get-next-music not implemented for cool-brain"))

;;; additional implementation

(defun cool-thinking (brain)
  "The main loop function for the thinking process of a `cool-brain`.
Should be called from a new thread."
  (loop while (cool-brain-thinking-p brain)
     do (progn
	  (thinking-step brain)
	  (sleep (cool-brain-interval brain)))))

(defun thinking-step (brain)
  "Simply thinks one thought."
  (with-slots (mind memory) brain
    ;;setting up the mind, if necessary
    (unless mind
      (setf mind (get-me-some-concepts memory)))
    (when mind
      (let* ((concept (select-concept brain))
	     (newcon (think-about concept brain))
	     (newweights (new-weights newcon brain)))
	(update-weights brain newweights)
	(update-mind brain)
	(when *debug-cool-brain*
	  (format t "~&~%Thinking step:~%mind: ~a~%memory: ~a items~%used concept: ~a~%resulting concept: ~a~%"
		  mind (hash-table-count memory) concept newcon))))))

(defun select-concept (brain)
  "=> a random concept from `mind`
selects a concept to think about from the mind"
  (with-slots (mind memory random-state) brain
    (let ((weights (mapcar (lambda (c) (gethash c memory 0)) mind)))
      (draw-weighted mind weights random-state))
    #|(labels ((w (c) (gethash c memory))
	     (sumup (lst acc)
	       (if lst
		   (if acc
		       (sumup (cdr lst) (cons (+ (car lst) (car acc)) acc))
		       (sumup (cdr lst) (cons (car lst) nil)))
		   acc))
	     (find-interval (lst n)
	       (if (or (null lst) (> n (car lst)))
		   (length lst)
		   (find-interval (cdr lst) n))))
      (let* ((weights (mapcar #'w mind))
	     (sums (sumup weights nil))
	     (n (find-interval sums (random (car sums) random-state))))
	(nth n mind)))|#))

(defun new-weights (concept brain &optional (path 1))
  "=> an alist of concepts and their weights
The concepts are the ones recursively related to `concept`.
The weights are the products of the relation weights starting from `concept`.
`concept` gets the weight `1`.
Each entry of the list has the form `(concept . weight)`.
Exploration is stopped, if a nodes weight falls below the value
of the `relation-threshold` slot of `brain`"
  (flet ((dig (rel)
	   (new-weights (car rel) brain (* path (cdr rel))))
	 (mult (p)
	   (lambda (rel)
	     (cons (car rel) (* p (cdr rel)))))
	 (merge-relations (rellsts)
	   (let ((rels (apply #'append rellsts))
		 (tbl (make-hash-table)))
	     (dolist (rel rels)
	       (let ((key (car rel))
		     (val (cdr rel)))
		 (setf (gethash key tbl)
		       (max val (gethash key tbl 0)))))
	     (hash-table-alist tbl))))
    (let* ((allrel (related-concepts concept brain))
	   (threshold (cool-brain-relation-threshold brain))
	   (relrel (remove-if (lambda (r)
				(< (* path (cdr r)) threshold))
			      allrel)))
      (when relrel
	(merge-relations (cons (mapcar (mult path) relrel)
			       (mapcar #'dig relrel)))))))

(defun update-weights (brain newweights)
  "Updates the weights of the concepts in `memory`,
considering the values in `newweights`"
  (with-slots (alpha memory) brain
    (dolist (rel newweights)
      (ensure-gethash (car rel) memory 1))
    (let ((newtbl (alist-hash-table newweights)))
      (hash-table-alist newtbl)
      (loop for c being the hash-keys in memory using (hash-value w)
	 do (setf (gethash c memory)
		  (+ (* alpha (gethash c newtbl 0))
		     (* (- 1 alpha) w)))))))

(defun update-mind (brain)
  "Decides, which concepts are to drop from or to push to the mind of `brain`."
  (with-slots (memory mind push-threshold drop-threshold) brain
    ;;remove uninteresting concepts
    (setf mind (remove-if (lambda (c)
			    (< (gethash c memory 0) drop-threshold))
			  mind))
    ;;add interesting concepts
    (loop for c being the hash-keys in memory using (hash-value w)
       do (when (> w push-threshold) (pushnew c mind)))))

(defun get-me-some-concepts (memory &optional (n *default-best-concepts-count*))
  "=> at most `n` concepts from `memory`"
  (let ((cs (hash-table-keys memory)))
      (loop for i from 1 to n while cs
	 collect (let ((re (random-elt cs)))
		   (setf cs (remove re cs))
		   re))))
