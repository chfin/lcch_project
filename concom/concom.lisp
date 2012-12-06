;;;; concom.lisp
;;;; Contains the main entry-points to concom (i.e. `improvise`, so far).

;;;; Copyright 2012 Christoph Finkensiep
;;;; This file is subject to the GUN General Public License version 3.
;;;; If a copy of the GNU GPLv3 was not distributed with this file,
;;;; You can obtain one at <http://www.gnu.org/licenses/>

(in-package #:concom)

(defvar *impro-running* nil)

(defun improvise (brain input &key (output *standard-output*))
  "This is the main entry-point for improvisation.
`brain` is the brain that should improvise.
`input` is some initial music input (topic of the improvisation).
If it is `nil`, the brain will use what ever is on its mind (literally).
`output` can be some music output channel (defaults to `*standard-output*` for textual output`)."
  (set-topic brain input)
  (start-thinking brain)
  (bordeaux-threads:make-thread
     (lambda ()
       (with-music-output (op output)
	 (setf *impro-running* t)
	 (loop while *impro-running*
	    do (progn
		 (enq-music op (get-next-music brain))
		 (sleep (- (remaining-time op) 0.1)))))
       (stop-thinking brain))))

(defun stop-impro ()
  (setf *impro-running* nil))
