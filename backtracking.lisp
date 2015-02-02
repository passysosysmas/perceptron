;;; Functions related to backtracking
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:perceptron)

(defun backtracking (network inputs output-error)
  ;;; first get the gradients of the network
  ;;; then apply corrections
  ;;; return corrected network
  (let ((network-gradients (network-gradients network inputs nil output-error)))
    (mapcar (lambda (layer layer-inputs layer-gradients)
	      (backtracking-layer layer layer-inputs layer-gradients))
	    network
	    inputs
	    network-gradients)))

;;; the backtracking apply corrections
(defun backtracking-layer (layer inputs layer-gradients)
  (mapcar (lambda (neuron gradient) (backtracking-neuron neuron inputs gradient))
	  layer
	  layer-gradients))

(defun backtracking-neuron (neuron inputs gradient)
  (push 1 inputs)
  (let ((delta-weights (mapcar (lambda (input)
				 (* *learning-rate* gradient input))  
			       inputs)))
    (mapcar #'+ neuron delta-weights)))

;;; the gradient functions calculates gradients through the network
(defun network-gradients (network inputs previous-layer previous-gradients)
  ;; the network is reversed because we start at the output layer
  (setq inputs (reverse inputs))
  (reverse (mapcar (lambda (layer layer-inputs)
		     (let ((layer-gradients (layer-gradients layer
							     layer-inputs
							     previous-layer
							     previous-gradients)))
		       (setq previous-layer layer)
		       (setq previous-gradients layer-gradients)
		       layer-gradients))
		   (reverse network)
		   inputs)))


(defun layer-gradients (layer layer-inputs previous-layer previous-gradients)
 (mapcar (lambda (neuron)
		(let ((gradient (neuron-gradient neuron
						 layer-inputs
						 (mapcar #'car previous-layer)
						 previous-gradients)))
		  (setq previous-layer (mapcar #'cdr previous-layer))
		  (when (not (car previous-layer))
		    (setq previous-layer nil))
		  gradient))
	 layer))


(defun neuron-gradient (neuron inputs previous-weights previous-gradients)
  (* (if previous-weights
	   (reduce #'+ (mapcar #'* previous-weights previous-gradients))
	   previous-gradients)
       (pd-activation-function (weighted-sum neuron inputs))))


;;; different activation function are available
;;; identity is terrible because the weighted-sum quickly gets way too high
(defun pd-activation-function (weighted-sum)
  (cond ((equal *activation-function* "identity") 1)
	('T (pd-logistic-function weighted-sum))))

(defun pd-logistic-function (weighted-sum)
  ;; pure maths
  ;; partial derivative of the logistic function
  (let ((temp (logistic-function weighted-sum)))
    (* temp (- 1 temp))))
