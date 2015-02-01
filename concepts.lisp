;;; Functions related to concepts
;;; File manipulation / get a concept to evaluate...

(in-package #:perceptron)

(defun refresh-stream (valid concept &optional testing)
  ;;; close and reopen a stream
  ;;; used when reaching EOF
  (if valid (close *concepts*) (close *not-concepts*))
  (if valid
      (setq *concepts*
	    (open (format nil "~a~a~a" *pathname* (if testing "test/" "") concept)
		  :element-type '(unsigned-byte 8)))
      (setq *not-concepts*
	    (open (format nil "~a~anot-~a" *pathname* (if testing "test/" "") concept)
		  :element-type '(unsigned-byte 8)))))

(defun refresh-streams (concept &optional testing)
  ;;; refreshing both streams at the same time
  ;;; used when testing (needs different set of concepts)
  (refresh-stream 'T concept testing)
  (refresh-stream nil concept testing))

(defun next-random-concept (concept positive-rate &optional testing)
  ;; returns a valid input of something representing the concept or not depending
  ;; of the random number. that way you can submit more right or more wrong by changing positive-rate
  (if (< (random 1.0) positive-rate)
      (next-concept T concept (if testing testing))
      (next-concept nil concept (if testing testing))))

(defun next-concept (valid concept &optional testing)
  ;; input : a concept like "a" or "0"
  ;; ugly part : feeds on external streams and reload them when reaching EOF
  ;; output : an adapted representation of a valid input representing the concept
  (let ((next-concept (make-sequence 'list 784)))
    (handler-case (when (eq 0 (read-sequence next-concept
					     (if valid *concepts* *not-concepts*)))
		    (refresh-stream valid concept (if testing testing))
		    (return-from  next-concept (next-concept valid concept (if testing testing))))
      (stream-error ()
	(refresh-stream valid concept (if testing testing))
	(read-sequence next-concept
		       (if valid *concepts* *not-concepts*))))
    (cons valid (mapcar (lambda (x) (/ x 255.0)) next-concept))))
