;;;; perceptron.lisp

(in-package #:perceptron)
(ql:quickload "split-sequence")

;;; Interface code
;;; exemple call :  (perceptron 1 0.2 10000 100)
(defun perceptron (hidden-neurons threshold learning-rate training-set testing-set)
  (defparameter *images* (open "~/lisp/perceptron/train-images"))
  (defparameter *labels* (open "~/lisp/perceptron/train-labels"))
  (defparameter *threshold* threshold)
  (defparameter *learning-rate* learning-rate)

  ;; Make a network of 2 layers : 784 inputs for the input layer
  ;;                              10 outputs for the output layer                       
  ;;(defparameter *network* (list (make-neuron-layer 784 hidden-neurons)
  ;;				(make-neuron-layer hidden-neurons 10)))
  (defparameter *network* (list (make-neuron-layer 784 hidden-neurons)))
				
  (training training-set)
  (print (error-rate testing-set))
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
  (position (reduce #'max results) results))

(defun error-rate (testing-set)
  (- 100 (* (/ (testing testing-set) testing-set) 100.0)))

(defun training (n)
  (let ((inputs nil))
    (dotimes (x n)
      (setq inputs (next-image *images*))
      (setq *network* (list (backtracking *network*
				          inputs
				          (compute-error (network-output *network*
								         inputs)
						         (label-to-results (next-label *labels*)))))))))
						       
(defun testing (n)
  (let ((success 0))
    (dotimes (x n)
      (when (equal (results-to-labels (network-output *network* (next-image *images*)))
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


;;; Backtracking
(defun backtracking (network input-values errors)
  (backtracking-layer (car network) input-values errors))

(defun backtracking-layer (layer input-values errors)
    (map 'vector
	 (lambda (neuron error) (backtracking-neuron neuron
						     input-values
						     error))
         layer
         errors))

(defun backtracking-neuron (neuron input-values error)
  (let ((pdaf (pdaf (weighted-sum neuron input-values))))
    (map 'vector
         (lambda (weight input-value) (+ weight
					 (delta-weight pdaf input-value error)))
	 neuron
	 input-values)))
				   
(defun compute-error (network-outputs expected-outputs)
  (map 'vector
       #'-
       network-outputs
       expected-outputs))

(defun delta-weight (pdaf input-value error)
  (* -1 *learning-rate* error pdaf input-value))
  
(defun pdaf (weighted-sum)
  ;; partial derivative of the activation function
  (let ((temp (activation-function weighted-sum)))
    (* temp (- 1 temp))))
