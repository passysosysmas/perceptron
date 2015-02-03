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

(defun next-random-concept (concept concepts positive-rate &optional testing)
  ;; returns a valid input of something representing the concept or not depending
  ;; of the random number. that way you can submit more right or more wrong by changing positive-rate
  (let ((concept (if (> (random 1.0) positive-rate)
		     (nth (random (length concepts)) concepts)
		     concept)))
    (cons concept (next-concept concept testing))))

(defun next-concept (concept testing)
  ;; input : a concept like "a" or "0"
  ;; ugly part : feeds on external streams and reload them when reaching EOF
  ;; output : an adapted representation of a valid input representing the concept
   (let ((next-concept (make-sequence 'list 784)))
     (handler-case (when (eq 0 (read-sequence next-concept
					      (gethash (format nil "~a~a" (if testing "test-" "")
							       concept)
							 *streams*)))
		     (refresh-stream concept testing)
		     (return-from next-concept (next-concept concept testing)))
       (type-error ()
	 (refresh-stream concept testing)
	 (read-sequence next-concept
			(gethash (format nil "~a~a" (if testing "test-" "")
							       concept)
						       *streams*))))
    (mapcar (lambda (x) (/ x 255.0)) next-concept)))



(defun unknown-concept-stream ()
  (defparameter *unknown* (open "~/lisp/perceptron/test-images-binary"
				:element-type '(unsigned-byte 8))))
(defun unknown-concept ()
  (unless *unknown* (setq *unknown* (unknown-concept-stream)))
  (let ((next-concept (make-sequence 'list 784)))
    (read-sequence next-concept *unknown*)
    (mapcar (lambda (x) (/ x 255.0)) next-concept)))
