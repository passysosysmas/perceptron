(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;;; perceptron.lisp
(in-package #:perceptron)

  
(defun exemple ()
  ;; an exemple of what can be done with the perceptron
  (let ((network-config '(784)))
    (defparameter *testing* nil)
    (format t "Learning rate  Sucess Rate~%")
    (loop
       for i from 1 to 8
       do (let ((test-output (testing
			      (perceptron (network network-config)
					  "0" 0.5 "logistic" 0.9 (/ i 10.0) 10000)
				"0"
				1000)))
	    (format t "~a ~15t~a%~%" (/ i 10.0) test-output)))
    (close *concepts*)
    (close *not-concepts*)))

;;test-number-recognition (numbers-recognition)
(defun test (number-recognition concept)
  (let ((maxi 0) (max-pos 0))
    (loop
       for element in (test-number-recognition number-recognition (cdr (test-concept 'T concept)))
       for i from 0 to 9
       do (when (> element maxi)
	   (setq maxi element)
	   (setq max-pos i)))
    max-pos))

(defun numbers-recognition ()
  (mapcar #'(lambda (concept)
	      (generator concept))
	  '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

(defun test-number-recognition (networks input)
  (mapcar (lambda (network) (car (network-output network input)))
	  networks))

(defun generator (concept)
  ;; test base function
  ;; at the moment it creates the necessary environment to generate a perceptron
  ;; the variables can be manipulated to test different output and configuration
  (defparameter *testing* nil)
  (let ((network-config '(784)))
    (perceptron (network network-config) concept 0.1 "logistic" 0.7 0.3 10000)))


(defun network (layers) 
  ;; input : list of the number of inputs for each neuron layer
  ;; output : a weights-network based on the input
  (if layers
      (cons (layer (first layers) ; the inputs of the current layer
		   (or (second layers) 1)) ; the outputs of the current layer
	    (network (rest layers))))) ;; the inputs of the remaining layers

(defun layer (inputs outputs)
  ;; a list of neurons
  (if (> outputs 0)
      (cons (neuron inputs)
	    (layer inputs (- outputs 1)))))

(defun neuron (inputs)
  ;; inputs : the number of inputs (weights) for the neuron
  ;; output : a list of weights (random) for each input
  (if (> inputs 0)
      (cons (- (random 1.0) 0.5)
	    (neuron (- inputs 1)))))

(defun perceptron (network concept momentum activation-function threshold learning-rate training-set)
  ;; inputs : configuration for a perceptron
  ;; outputs : prints statistics
  ;; TODO : improve behavior and documentation
  (defparameter *pathname* "lisp/perceptron/images/")
  (defparameter *concepts* (open (format nil "~a~a" *pathname* concept)))
  (defparameter *not-concepts* (open (format nil "~anot-~a" *pathname* concept))) 
  (defparameter *momentum* momentum)
  (defparameter *activation-function* activation-function)
  (defparameter *threshold* threshold)
  (defparameter *learning-rate* learning-rate)
  (defparameter *bias* -1)
  (let ((network (training network concept training-set)))
    (close *concepts*)
    (close *not-concepts*)
    network))


(defun training (network concept n)
  ;; inputs : a network and a concept, n the number of cycles
  ;; runs n time the backtracking algorithm on a compatible representation of a
  ;; representation of the concept AND a compatible representation of something that is
  ;; not a representation of the concept, using new reprensations at each cycle.
  ;; outputs : a trained network
  (with-open-file (file (format nil "lisp/perceptron/quadratic-error/~a-~a-~a.csv"
				concept n *learning-rate*)
			:direction :output :if-exists :overwrite :if-does-not-exist :create )
    (let ((quadratic-error-sum 0))
      (dotimes (x n)
	;; pick a random concept with 50% chances to get a true one
	(let ((random-concept (next-random-concept concept 0.5)))
	  ;; save network-output for this concept
	  (let ((network-output (network-output network (cdr random-concept))))
	    ;; add error to quadratic error sum
	    (incf quadratic-error-sum
		  (quadratic-error (- (car network-output)
				      (if (car random-concept) 1 0.5))))
	    ;; save medium QE to a file 
	    (format file "~a,~a" (sqrt (/ quadratic-error-sum (+ 1 x)))
		    (network-error (car network-output) (car random-concept)))
	    (write-char #\linefeed file)
	    ;; start backtracking
	    (setq network
		  (backtracking network
				(append (list (cdr random-concept))
					(cdr network-output))
				(network-error (car network-output) (car random-concept))))))))
    network))



(defun testing (network concept n)
  ;; inputs : a trained network and a concept, n the number of tests to do
  ;; runs t time and compares the output of the network when submitted a compatible representation
  ;; of the concept and a valid representation of somethingg that is not a representation of the
  ;; concept
  ;; outputs : the error rate on the testing set
  (setq *testing* "test/")
  (refresh-streams concept)
  (let ((success 0))
    (dotimes (x n)
      (let ((random-concept (next-random-concept concept 0.5)))
	(let ((network-output (car (network-output network (cdr random-concept)))))
	  (if (car random-concept)
	      (when (> network-output *threshold*)
		(incf success))
	      (when (< network-output *threshold*)
		(incf success))))))
    (setq  *testing* nil)
    (refresh-streams concept)
    (* (- 1 (/ success n)) 100.0)))

(defun network-error (network-output concept)
  (- (if concept 1 0)
     network-output))

(defun network-output (network input)
  ;; returns the output of the network for a particular input consed with
  ;; the detailed output for each layer
  (let ((network-output (detailed-network-output network input)))
    (cons (caar (last network-output)) network-output)))

(defun detailed-network-output (network input-values)
  ;; returns the detailed-output
  ;; usefull for backtracking
  (mapcar (lambda (layer) (setq input-values (layer-output layer input-values)))
	  network))

(defun layer-output (layer input)
  ;; input a layer of a neuron network and valid input
  ;; output : a list of outputs for every neuron of the layer
  (mapcar (lambda (neuron) (neuron-output neuron input))
	  layer))  

(defun neuron-output (neuron input)
  ;; input a list of weights that actually are the representation of a neuron and valid input
  ;; output : the output of the neuron
  (activation-function (weighted-sum neuron input)))


(defun weighted-sum (neuron input)
  ;; input a list of weights and valid input
  ;; hadoop or whatever distribited magic should be used here and there for the rise of Skynet
  ;; output : the weighted sum
  (+ *bias* (reduce #'+ (mapcar #'* neuron input))))

(defun activation-function (weighted-sum)
  (cond ((equal *activation-function* "identity") weighted-sum)
	('T (logistic-function weighted-sum))))

(defun logistic-function (weighted-sum)
  ;; pure math
  ;; conditionals are here to avoid overflow when weighted-sum is too small
  (cond ((> weighted-sum 50) 1.0)
	((< weighted-sum -50) 0.0)
	('T (/ 1 (+ 1 (exp (* -1 weighted-sum)))))))

(defun quadratic-error (neuron-error)
  (/ (expt neuron-error 2) 2))





