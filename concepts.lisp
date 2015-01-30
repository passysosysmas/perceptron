;;; Functions related to concepts
;;; File manipulation / get a concept to evaluate...

(in-package #:perceptron)

(defun test-concept (valid concept)
  (defparameter *concepts* (open (format nil "~a~a" *pathname* concept)))
  (defparameter *not-concepts* (open (format nil "~anot-~a" *pathname* concept))) 
  (let ((next-concept (next-concept valid concept)))
    (close *concepts*)
    (close *not-concepts*)
    next-concept))
  )

(defun refresh-stream (valid concept)
  ;;; close and reopen a stream
  ;;; used when reaching EOF
  (if valid (close *concepts*) (close *not-concepts*))
  (if valid
      (setq *concepts*
	    (open (format nil "~a~a~a" *pathname* (or *testing* "") concept)))
      (setq *not-concepts*
	    (open (format nil "~a~anot-~a" *pathname* (or *testing* "") concept)))))

(defun refresh-streams (concept)
  ;;; refreshing both streams at the same time
  ;;; used when testing (needs different set of concepts)
  (refresh-stream 'T concept)
  (refresh-stream nil concept))

(defun next-random-concept (concept positive-rate)
  ;; returns a valid input of something representing the concept or not depending
  ;; of the random number. that way you can submit more right or more wrong by changing positive-rate
  (if (< (random 1.0) positive-rate)
      (next-concept T concept)
      (next-concept nil concept)))


(defun next-concept (valid concept)
  ;; input : a concept like "a" or "0"
  ;; ugly part : feeds on external streams and reload them when reaching EOF
  ;; output : an adapted representation of a valid input representing the concept
  (let ((next-concept nil))
    (handler-case (setq next-concept (read-line (if valid *concepts* *not-concepts*)))
      (stream-error ()
	(refresh-stream valid concept)
	(setq next-concept (read-line (if valid *concepts* *not-concepts*)))))
    (cons valid (next-concept-extraction-image next-concept))))


(defun next-concept-extraction-image (next-concept)
  ;; input : a representation of a concept (string of 784 space separated integers between 0 and 255)
  ;; output : a representation of a concept (vector of 784 real numbers between 0 and 1)
  (mapcar #'(lambda (x) (/ (parse-integer x) 255.0))
	  (split-sequence:split-sequence #\Space
					 next-concept
					 :remove-empty-subseqs t)))

(defun next-concept-extraction-bool (next-concept)
  ;; input : a representation of a concept (string of 784 space separated integers between 0 and 255)
  ;; output : a representation of a concept (vector of 784 real numbers between 0 and 1)
  (mapcar #'(lambda (x) (parse-integer x))
	  (split-sequence:split-sequence #\Space
					 next-concept
					 :remove-empty-subseqs t)))
