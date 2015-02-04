(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; Functions related to concepts
;;; File manipulation / get a concept to evaluate...

(in-package #:perceptron)

(defun open-streams (concepts &optional testing)
  (defparameter *streams* (make-hash-table :test 'equal))
  (loop for concept in concepts
     do (setf (gethash (format nil "~a~a" (if testing "test-" "") concept)
		       *streams*)
	      (open (format nil "~a~a~a" *pathname* (if testing "test/" "") concept)
		    :element-type '(unsigned-byte 8)))))

(defun refresh-stream (concept testing)
  ;;; close and reopen a stream
  ;;; used when reaching EOF
  (setf (gethash (format nil "~a~a" (if testing "test-" "") concept)
		    *streams*)
	(open (format nil "~a~a" *pathname* concept)
	      :element-type '(unsigned-byte 8))))

(defun next-random-concept (perceptron positive-rate &optional mode)
  ;; returns a valid input of something representing the concept or not depending
  ;; of the random number. that way you can submit more right or more wrong by changing positive-rate
  (if (> (random 1.0) positive-rate)
      (or (when (member  mode '(recall review)) (pop (p-false-positive-concepts perceptron)))
	  (next-concept (nth (random (length (p-concepts perceptron)))
			     (p-concepts perceptron))
			(eq mode 'testing)))
      (or (when (member mode '(precision review)) (pop (p-false-negative-concepts perceptron)))
	  (next-concept (p-concept-label perceptron)
			(eq mode 'testing)))))

(defun next-concept (concept-label testing)
  ;; input : a concept
  ;; testing, relearning : booleans
  ;; ugly part : feeds on external streams and reload them when reaching EOF
  ;; output : an adapted representation of a valid input representing the concept
  (let ((next-concept (make-sequence 'list 784)))
    (handler-case (when (eq 0 (read-sequence next-concept
					     (gethash (format nil "~a~a" (if testing "test-" "")
							      concept-label)
						      *streams*)))
		    (refresh-stream concept-label testing)
		    (return-from next-concept
		      (next-concept concept-label testing)))
      (type-error ()
	(refresh-stream concept-label testing)
	(read-sequence next-concept
		       (gethash (format nil "~a~a" (if testing "test-" "")
					concept-label)
				*streams*))))
    (cons concept-label (mapcar (lambda (x) (/ x 255.0)) next-concept))))



(defun unknown-concept-stream ()
  (defparameter *unknown* (open "~/lisp/perceptron/test-images-binary"
				:element-type '(unsigned-byte 8))))

(defun unknown-concept (&optional stream)
  (unless stream (setq stream *unknown*))
  (let ((next-concept (make-sequence 'list 784)))
    (read-sequence next-concept stream)
    (mapcar (lambda (x) (/ x 255.0)) next-concept)))
