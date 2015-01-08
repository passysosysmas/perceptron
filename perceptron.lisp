;;;; perceptron.lisp
(ql:quickload "split-sequence")
(in-package #:perceptron)


;;; Interface code
;;; exemple call :  (perceptron 1 0.2 10000 100)
(defun perceptron (hidden-neurons threshold learning-rate training-set testing-set)
  (defparameter *images* (open "~/lisp/perceptron/train-images"))
  (defparameter *labels* (open "~/lisp/perceptron/train-labels"))
  (defparameter *threshold* threshold)
  (defparameter *learning-rate* learning-rate)

  ;; Make a network of 2 layers : 784 inputs for the input layer
  ;;                              10 outputs for the output layer                       
  (let ((network (list (make-neuron-layer 784 hidden-neurons)
  		       (make-neuron-layer hidden-neurons 10))))
  ;;(let ((network (list(make-neuron-layer 784 hidden-neurons))))
    (print "TRAINING")
    (time (setq network (training network training-set)))
    (print "TESTING")
    (time (print (error-rate network testing-set))))

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
  (let ((results (make-array 10 :initial-element 0)))
    (setf (aref results label) 1)
    results))

(defun outputs-to-results (outputs)
  (map 'vector
       (lambda (output) (if (> output *threshold*)
			     't
			     nil))
       outputs))

(defun results-to-labels (results)
  ;; the number with the highest output is considered to be the right answer
  ;; TODO: low priority, probably suboptimal alogrithm but only runned once/test
  (position (reduce #'max results) results))

(defun error-rate (network testing-set)
  (- 100 (* (/ (testing network testing-set) testing-set) 100.0)))

(defun training (network n)
  (let ((inputs nil))
    (dotimes (x n)
      (setq inputs (next-image *images*))
      (setq network (backtracking  network
				   inputs
				   (network-output network inputs)
				   (label-to-results (next-label *labels*))))))
  network)

(defun testing (network n)
  (let ((success 0))
    (dotimes (x n)
      (when (equal (results-to-labels (network-output network (next-image *images*)))
		   (next-label *labels*))
	(setq success (+ success 1))))
    success))


;;; Business code
(defun make-neuron (inputs)
  (make-array inputs :initial-element 0))

(defun make-neuron-layer (inputs outputs)
  (make-array outputs :initial-element (make-neuron inputs)))

(defun network-output (network input-values)
  (if (cdr network)
    (network-output (cdr network) (layer-output (car network) input-values))
    (layer-output (car network) input-values)))

(defun layer-output (layer input-values)
  (map 'vector
       (lambda (neuron) (neuron-output neuron input-values))
       layer))  

(defun neuron-output (weights input-values)
  (activation-function (weighted-sum weights input-values)))

(defun weighted-sum (weights input-values)
  (reduce '+ (map 'vector '* input-values weights)))

(defun activation-function (weighted-sum)
  ;; Using logistic function
  (/ 1 (+ 1 (exp (* -1 weighted-sum)))))

(defun output-error (output-values expected-values)
  (list(map 'vector
       #'-
       output-values
       expected-values)))
  
;;; Network : list of layers
;;; Layer : vector of neurons
;;; Neuron : vector of weights

;;; Backtracking
(defun backtracking (network input-values output-values expected-values)
  (map 'vector
       #'(lambda (layer error-network)
	   (map 'vector #'(lambda (neuron error-layer)
			       (map 'vector #'(lambda (weight input-value error-neuron)
						(+ weight (* -1
						             *learning-rate*
						             error-neuron
						             input-value)))
				    neuron
				    input-values
				    error-layer))
		     layer
		     error-network))
	  network
	  (backtracking-network network
				input-values
				(output-error output-values expected-values))))))

(defun backtracking-network (network input-values errors)
  (let ((backtracking (backtracking-layer (car network)
					  input-values
					  (car errors))))
    (if (cadr network)
      (push (backtracking-network (cdr network)
				  (layer-output (car network)
					        input-values)
				  backtracking)
	    backtracking)
      (list backtracking))))
      



(defun backtracking-layer (layer input-values errors)
  (print errors)
    (map 'vector
	 (lambda (neuron error) (backtracking-neuron neuron
						     input-values
						     error))
         layer
         errors))

(defun backtracking-neuron (neuron input-values error)
  ;; error multiplied by the partial derivative of the activation function
  (* error (pdaf (weighted-sum neuron input-values)))))
  
(defun pdaf (weighted-sum)
  ;; partial derivative of the activation function
  (let ((temp (activation-function weighted-sum)))
    (* temp (- 1 temp))))
