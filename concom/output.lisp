;;;; output.lisp

(in-package #:concom)

(defvar *default-remaining-time* 1.0)

(defgeneric start-output (output)
  (:documentation "Allows the output medium `output` to prepare itself."))
(defgeneric stop-output (output)
  (:documentation "Stops the output medium `output`."))

(defgeneric enq-note (output note)
  (:documentation "Enqueues a single note to be played/processed by `output`."))
(defgeneric enq-music (output music)
  (:documentation "Enqueues a list of notes to `output`"))

(defgeneric remaining-music-time (output)
  (:documentation "=> the time (in s) that remains, until the queue is empty.
What you really want is `remaining-time`"))

(defun remaining-time (output)
  "=> the time (in s) that remains, until the queue of `output` is empty
`remaining-music-time` is used to determine this value.
If it returns `nil` the value of `*defaul-remaining-time*` is used (e.g. for text output only)."
  (or (remaining-music-time output)
      *default-remaining-time*))

(defmacro with-music-output ((var &rest outputs) &body body)
  `(let ((,var ,(if (<= (length outputs) 1)
		    (car outputs)
		    `(make-mmo ,@outputs))))
     (start-output ,var)
     (unwind-protect (progn ,@body)
       (stop-output ,var))))

;;; text output to streams

(defmethod start-output ((output stream))
  (princ "Starting music output." output)
  (fresh-line output))

(defmethod stop-output ((output stream))
  (princ "Stopping music output." output)
  (fresh-line output))

(defmethod enq-note ((output stream) note)
  (princ note output)
  (fresh-line output))

(defmethod enq-music ((output stream) music)
  (format output "~{~a~^ ~}~%" music)
  t)

(defmethod remaining-music-time ((output stream))
  nil)

;;; an output multiplexer

(defclass multi-music-output ()
  ((outputs :initarg :outputs
	    :initform nil
	    :reader mmo-outputs)))

(defun make-mmo (&rest outputs)
  (make-instance 'multi-music-output :outputs outputs))

(defmethod start-output ((output multi-music-output))
  (mapcar #'start-output (mmo-outputs output)))

(defmethod stop-output ((output multi-music-output))
  (mapcar #'stop-output (mmo-outputs output)))

(defmethod enq-note ((output multi-music-output) note)
  (mapcar (lambda (o) (enq-note o note)) (mmo-outputs output)))

(defmethod enq-music ((output multi-music-output) music)
  (mapcar (lambda (o) (enq-music o music)) (mmo-outputs output)))

(defmethod remaining-music-time ((output multi-music-output))
  (find-if-not #'null (mapcar #'remaining-music-time (mmo-outputs output))))

;;; lilypond file output
;;; TODO!
