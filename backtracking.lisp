;;; Functions related to backtracking
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:perceptron)

(defun backtracking (perceptron inputs output-error)
  ;;; first get the gradients of the network
  ;;; then apply corrections
  ;;; return corrected network
  (let ((network-gradients (network-gradients perceptron inputs nil output-error)))
    (mapcar (lambda (layer layer-inputs layer-gradients)
	      (backtracking-layer layer perceptron layer-inputs layer-gradients))
	    (p-network perceptron)
	    inputs
	    network-gradients)))

;;; the backtracking apply corrections
(defun backtracking-layer (layer perceptron inputs layer-gradients)
  (mapcar (lambda (neuron gradient) (backtracking-neuron neuron perceptron inputs gradient))
	  layer
	  layer-gradients))

(defun backtracking-neuron (neuron perceptron inputs gradient)
  (push 1 inputs)
  (let ((delta-weights (mapcar (lambda (input)
				 (* (p-learning-rate perceptron) gradient input))  
			       inputs)))
    (mapcar #'+ neuron delta-weights)))

;;; the gradient functions calculates gradients through the network
(defun network-gradients (perceptron inputs previous-layer previous-gradients)
  ;; the network is reversed because we start at the output layer
  (setq inputs (reverse inputs))
  (reverse (mapcar (lambda (layer layer-inputs)
		     (let ((layer-gradients (layer-gradients layer
							     perceptron
							     layer-inputs
							     previous-layer
							     previous-gradients)))
		       (setq previous-layer layer)
		       (setq previous-gradients layer-gradients)
		       layer-gradients))
		   (reverse (p-network perceptron))
		   inputs)))


(defun layer-gradients (layer perceptron layer-inputs previous-layer previous-gradients)
 (mapcar (lambda (neuron)
	   (let ((gradient (neuron-gradient neuron
					    perceptron
					    layer-inputs
					    (mapcar #'car previous-layer)
					    previous-gradients)))
	     (setq previous-layer (mapcar #'cdr previous-layer))
	     (when (not (car previous-layer))
	       (setq previous-layer nil))
	     gradient))
	 layer))

;;; TODO : put activation function in the config and call it with funcall
(defun neuron-gradient (neuron perceptron inputs previous-weights previous-gradients)
  (* (if previous-weights
	   (reduce #'+ (mapcar #'* previous-weights previous-gradients))
	   previous-gradients)
       (pd-logistic-function (weighted-sum neuron inputs))))

(defun pd-logistic-function (weighted-sum)
  ;; pure maths
  ;; partial derivative of the logistic function
  (let ((temp (logistic-function weighted-sum)))
    (* temp (- 1 temp))))
