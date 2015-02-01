(declaim (optimize (speed 0) (safety 3) (debug 3)))
;; add evolutionary strategies for learning rate

;;;; perceptron.lisp
(in-package #:perceptron)

(defun main ()
  ;;;'("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
  (defparameter *verbose* nil)
  (defparameter *pathname* "lisp/perceptron/images/")
  (let ((concepts  '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
    (defparameter *concepts* (open (format nil "~atest/~a" *pathname* (car concepts))))
    (defparameter *not-concepts* (open (format nil "~atest/not-~a" *pathname* (car concepts))))
    (let ((networks-set (networks-set concepts)))
      (testing-networks-set networks-set concepts 100)
      (defparameter *networks-set* networks-set))))

(defun networks-set (concepts)
  ;;; generates a set of 10 perceptrons for the network
  (let* ((network-config '(784))
	 (activation-function "logistic")
	 (learning-rates '(0.5 0.3))
	 (training-set 1000) (testing-set 100)
	 (threshold 0.8) (quadratic-limit 0.07))
    (mapcar (lambda (concept)
		(perceptron (best-perceptron network-config concept activation-function
					     learning-rates threshold training-set
					     testing-set quadratic-limit)
			    concept activation-function learning-rates threshold
			    training-set testing-set quadratic-limit))
	    concepts)))

(defun network (layers) 
  ;;; input : list of the number of inputs for each neuron layer
  ;;; output : a weights-network based on the input
  (if layers
      (cons (layer (first layers) ; the inputs of the current layer
		   (or (second layers) 1)) ; the outputs of the current layer
	    (network (rest layers))))) ; the inputs of the remaining layers

(defun layer (inputs outputs)
  ;;; a list of neurons
  (if (> outputs 0)
      (cons (neuron (+ 1 inputs))
	    (layer inputs (- outputs 1)))))

(defun neuron (inputs)
  ;;; inputs : the number of inputs (weights) for the neuron
  ;;; output : a list of weights (random) for each input
  (if (> inputs 0)
      (cons (random 1.0 )
	    (neuron (- inputs 1)))))

(defun best-perceptron (network-config concept activation-function learning-rates threshold
			training-set testing-set quadratic-limit)
  ;;; an exemple of what can be done with the perceptron
  (let ((best-network nil) (best-error-rate 1000))
    (dotimes (x 10)
      (let* ((current-network (perceptron (network network-config)
					  concept activation-function learning-rates threshold
					  training-set testing-set quadratic-limit))
	     (current-error-rate (testing-perceptron current-network concept threshold testing-set)))
	(when (< current-error-rate best-error-rate)
	  (setq best-error-rate current-error-rate)
	  (setq best-network current-network))))
    (perceptron best-network
		concept activation-function '(0.3) threshold
		training-set testing-set quadratic-limit)
    best-network))

(defun perceptron (network concept activation-function learning-rates threshold
		   training-set testing-set &optional quadratic-limit)
  ;;; inputs : configuration for a perceptron
  ;;; outputs : trained network
  (defparameter *activation-function* activation-function)
  (loop for learning-rate in learning-rates
     do (setq network (training-perceptron network
					   concept
					   learning-rate
					   training-set
					   quadratic-limit))
       (when *verbose*
	 (format t "Error rate of ~a% with learning rate of ~a~%"
		 (testing-perceptron network concept threshold testing-set)
		 learning-rate)))
  network)

(defun training-perceptron (network concept learning-rate n &optional (quadratic-limit 0))
  ;;; inputs : a network and a concept, n the number of cycles
  ;;; runs n time the backtracking algorithm on a compatible representation of a
  ;;; representation of the concept AND a compatible representation of something that is
  ;;; not a representation of the concept, using new reprensations at each cycle.
  ;;; outputs : a trained network
  (defparameter *learning-rate* learning-rate)
  (with-open-file (file (format nil "lisp/perceptron/quadratic-error/~a-~a-~a.csv"
				concept n *learning-rate*)
			:direction :output :if-exists :overwrite :if-does-not-exist :create )
    (let ((quadratic-error-sum 0))
      (dotimes (x n)
	(let* ((random-concept (next-random-concept concept 0.5))
	       (network-output (network-output network (cdr random-concept)))
	       (network-error (network-error (car network-output) (car random-concept)))
	       (quadratic-error (quadratic-error (car network-output) (car random-concept)))
	       (sqrt-mqe (sqrt-mqe quadratic-error-sum x)))
	  (incf quadratic-error-sum quadratic-error)
	  ;; save medium QE to a file
	  (format file "~a,~a~%"  sqrt-mqe network-error)
	  ;; start backtracking
	  (setq network
		(backtracking network
			      (append (list (cdr random-concept))
				      (cdr network-output))
			      network-error))
	  (when (and (> x 100) (> quadratic-limit sqrt-mqe))
	    (when *verbose*
	      (format t "Quadratic limit reached~%Iterations: ~a~%" x))
	    (return network))))
      (when *verbose*
	(format t "Max iterations reached~%(sqrt MQE) : ~a~%~%"
		(sqrt-mqe quadratic-error-sum n)))))
  network)


(defun testing-networks-set (networks-set concepts n)
  ;; inputs : a trained network and a concept, n the number of tests to do
  ;; runs t time and compares the output of the network when submitted a compatible representation
  ;; of the concept and a valid representation of somethingg that is not a representation of the
  ;; concept
  ;; outputs : the error rate on the testing set
  (mapcar (lambda (concept)
	    (refresh-streams concept 'testing)
	    (let ((success 0))
	      (dotimes (x n)
		(let* ((next-concept (next-concept 'T concept 'testing))
		      (network-output (networks-set-output networks-set
							     concepts
							     (cdr next-concept))))
		  (when (equal network-output concept)
		    (incf success))))
	      (when t
		(format t "Concept: ~a~12t Error rate: ~a%~%"
			concept (* (- 1 (/ success n)) 100.0)))))
	  concepts))

(defun testing-perceptron (network concept threshold n)
  ;;; inputs : a trained network and a concept, n the number of tests to do
  ;;; runs t time and compares the output of the network when submitted a compatible representation
  ;;; of the concept and a valid representation of somethingg that is not a representation of the
  ;;; concept
  ;;; outputs : the error rate on the testing set
  (refresh-streams concept 'testing)
  (let ((success 0))
    (dotimes (x n)
      (let* ((next-concept (next-random-concept concept 0.5 'testing))
	     (network-output (car (network-output network (cdr next-concept)))))
	(if (car next-concept)
	    (when (> network-output threshold)
	      (incf success))
	    (when (< network-output threshold)
	      (incf success)))))
    (* (- 1 (/ success n)) 100.0)))

(defun networks-set-output (networks-set concepts input)
  (when *verbose* (display-image input))
  (let ((outputs (mapcar (lambda (output concept)
			   (when *verbose*
			     (format t "Concept ~a : ~a%~%" concept
						   (if (> output 0.0001) (* 100 output) "< 0.01")))
			   output)
		 (mapcar (lambda (network)
			   (car (network-output network input)))
			 networks-set)
		 concepts)))
    (nth (position (reduce #'max outputs) outputs) concepts)))

(defun network-output (network input)
  ;;; returns the output of the network for a particular input consed with
  ;;; the detailed output for each layer
  (let ((network-output (detailed-network-output network input)))
    (cons (caar (last network-output)) network-output)))

(defun detailed-network-output (network input-values)
  ;;; returns the detailed-output
  ;;; usefull for backtracking
  (mapcar (lambda (layer) (setq input-values (layer-output layer input-values)))
	  network))

(defun layer-output (layer input)
  ;;; input a layer of a neuron network and valid input
  ;;; output : a list of outputs for every neuron of the layer
  (mapcar (lambda (neuron) (neuron-output neuron input))
	  layer))  

(defun neuron-output (neuron input)
  ;;; input a list of weights that actually are the representation of a neuron and valid input
  ;;; output : the output of the neuron
  (activation-function (weighted-sum neuron input)))


(defun weighted-sum (neuron input)
  ;;; input a list of weights and valid input
  ;;; hadoop or whatever distribited magic should be used here and there for the rise of Skynet
  ;;; output : the weighted sum
  (push 1 input )
  (+ (reduce #'+ (mapcar #'* neuron input))))

(defun activation-function (weighted-sum)
  (cond ((equal *activation-function* "identity") weighted-sum)
	('T (logistic-function weighted-sum))))

(defun logistic-function (weighted-sum)
  ;;; pure math
  ;;; conditionals are here to avoid overflow when weighted-sum is too small
  (cond ((> weighted-sum 50) 1.0)
	((< weighted-sum -50) 0.0)
	('T (/ 1 (+ 1 (exp (* -1 weighted-sum)))))))

(defun network-error (network-output concept)
 (- (if concept 1 0)
	      network-output)) 

(defun quadratic-error (network-output concept)
  (/ (expt (- (if concept 1 0)
	      network-output)
	   2)
     2))

(defun sqrt-mqe (quadratic-error-sum x)
  (sqrt (/ quadratic-error-sum (+ 1 x))))


;;; EXEMPLES

;;; To show the output of the networks-set for a given input :
;;;(networks-set-output *networks-set* '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9") (cdr (next-concept T "8")))

;;; To see how ugly the images are :
;;; (loop for i from 1 to 10 do (display-image (cdr (next-concept T "8"))))
