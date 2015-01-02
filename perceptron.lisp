;;;; perceptron.lisp

(in-package #:perceptron)
(ql:quickload "split-sequence")

;;; Interface code
;;; exemple call :  (perceptron 1 0.2 10000 100)
(defun perceptron (threshold learning-rate training-set testing-set)
  (defparameter *images* (open "~/lisp/perceptron/train-images"))
  (defparameter *labels* (open "~/lisp/perceptron/train-labels"))

  (defparameter *threshold* threshold)
  (defparameter *learning-rate* learning-rate)

  (let ((network (make-neuron-layer 784 10)))
    ;;training
    (print (testing (training network training-set)
		    testing-set)))
    ;;testing
    ;;(print (
  (close *images*)
  (close *labels*))

;;; DO NOT USE ALONE when testing
(defun next-image (images)
  (map 'vector
       #'(lambda (x) (/ (parse-integer x) 255.0))
       (split-sequence:split-sequence #\Space
				      (read-line images)
				      :remove-empty-subseqs t)))

;;; DO NOT USE ALONE, use the 'next function instead to keep images and labels in sync
(defun next-label (label-images)
  (parse-integer (read-line label-images)))

(defun label-to-results (label)
  (let ((results (make-array 10 :initial-element NIL)))
    (setf (aref results label) T)
    results))

(defun results-to-labels (results)
  (loop
     for item across results
     for x from 0
     when item collect x)))

(defun error-rate (success testing-set)
  (- 100 (* (/ success testing-set) 100.0)))

(defun training-rec (network training-set)
  (when (> training-set 0)
    (training (train-network network
			     (next-image *images*)
			     (label-to-results (next-label *labels*)))
	      (- training-set 1))))

(defun training (temp training-set)
  (let ((network temp))
    (dotimes (x training-set)
      (setq network (train-network network
			           (next-image *images*)
			           (label-to-results (next-label *labels*)))))
    network))

(defun testing (network testing-set)
  (let ((success 0))
    (dotimes (x testing-set)
      (when (equal (label-to-results (next-label *labels*))
		   (network-output network
				   (next-image *images*)))
        (setq success (+ 1 success))))
    (print success)
    (error-rate success testing-set)))

;;; Business code
(defun make-neuron (inputs)
  (make-array inputs :initial-element 0))

(defun make-neuron-layer (inputs outputs)
  (make-array outputs :initial-element (make-neuron inputs)))

(defun train-neuron (weights input-values result)
  (let ((output (neuron-output weights input-values))
	(operator nil))
    ;; determining the type of correction needed
    (cond ((and result (not output)) (setq operator #'+))
	  ((and output (not result)) (setq operator #'-)))
    ;;applying correction if needed
    (if operator
      (map 'vector operator
	   weights
           (map 'vector (lambda (x) (* *learning-rate* x)) input-values))
      weights)))

(defun neuron-output (weights input-values)
  (> (reduce '+ (map 'vector '* input-values weights)) *threshold*))

(defun train-network (network input-values results)
  (map 'vector
       (lambda (neuron result) (train-neuron neuron input-values result))
       network
       results))

(defun network-output (network input-values)
  (map 'vector
       (lambda (neuron) (neuron-output neuron input-values))
       network))
