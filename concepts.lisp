(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; Functions related to concepts
;;; File manipulation / get a concept to evaluate...

(in-package #:perceptron)

(defun open-streams (concepts)
  (defparameter *concepts* (mapcar (lambda (concept)
				    (open (format nil "~a~a" *pathname* concept)
					  :element-type '(unsigned-byte 8)))
				   concepts))
  (defparameter *testing-concepts* (mapcar (lambda (concept)
					     (open (format nil "~atest/~a" *pathname* concept)
						   :element-type '(unsigned-byte 8)))
					   concepts)))

(defun close-streams ()
  (mapcar #'close *concepts*)
  (mapcar #'close *testing-concepts*))

(defun refresh-stream (concepts concept-position &optional testing)
  ;;; close and reopen a stream
  ;;; used when reaching EOF
  (let ((stream (if testing *testing-concepts* *concepts*)))
    (close (nth concept-position stream))
    (setf (nth concept-position stream)
	  (open (format nil "~a~a~a" *pathname* (if testing "test/" "") (nth concept-position
									     concepts))
		:element-type '(unsigned-byte 8)))))

(defun get-stream (concept-position &optional testing)
  (nth concept-position (if testing *testing-concepts* *concepts*)))

(defun next-random-concept (concepts concept-position positive-rate &optional testing)
  ;; returns a valid input of something representing the concept or not depending
  ;; of the random number. that way you can submit more right or more wrong by changing positive-rate
  (when (> (random 1.0) positive-rate)
    (setf concept-position (random (length concepts))))
  (next-concept concepts concept-position (if testing testing)))

(defun next-concept (concepts concept-position &optional testing)
  ;; input : a concept like "a" or "0"
  ;; ugly part : feeds on external streams and reload them when reaching EOF
  ;; output : an adapted representation of a valid input representing the concept
  (let ((next-concept (make-sequence 'list 784)))
    (handler-case (when (eq 0 (read-sequence next-concept
					     (get-stream concept-position (if testing testing))))
		    (refresh-stream concepts concept-position (if testing testing))
		    (return-from  next-concept (next-concept concepts concept-position
							     (if testing testing))))
      (stream-error ()
	(refresh-stream concepts concept-position (if testing testing))
	(read-sequence next-concept
		       (get-stream concept-position (if testing testing)))))
    (cons concept-position (mapcar (lambda (x) (/ x 255.0)) next-concept))))

(defun unknown-concept-stream ()
  (defparameter *unknown* (open "~/lisp/perceptron/test-images-binary"
				:element-type '(unsigned-byte 8))))

(defun unknown-concept ()
  (unless *unknown* (setq *unknown* (unknown-concept-stream)))
  (let ((next-concept (make-sequence 'list 784)))
    (read-sequence next-concept *unknown*)
    (mapcar (lambda (x) (/ x 255.0)) next-concept)))
