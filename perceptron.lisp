(declaim (optimize (speed 0) (safety 3) (debug 3)))
;; add evolutionary strategies for learning rate

;;;; perceptron.lisp
(in-package #:perceptron)

;;;; TODO separate the different layers, have the higher level
;;;; functions in a separate file for future evolutions

(defun bootstraping ()
  ;;; you need to put the image files into separated folders (see README.txt)
  (generate-files))

(defun main ()
  (defparameter *concepts-labels* '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (defparameter *verbose* nil)
  (defparameter *pathname* "lisp/perceptron/images/")
  (let ((network-config '(784))
	(concepts  *concepts-labels*)
	(activation-function "logistic")
	(learning-rates '(0.05))
	(training-set 1000000) (testing-set 1000)
	(threshold 0.8) (quadratic-limit 0.2))
    (open-streams concepts)
    (let ((networks-set (networks-set network-config concepts activation-function
				      learning-rates threshold training-set
				      testing-set quadratic-limit)))
      (testing-networks-set networks-set concepts testing-set)
      (defparameter *networks-set* networks-set))
    (network-to-file)
    (close *unknown*)
    (unknown-concept-stream)
    (close-streams)))

(defun test-concept ()
  (setf *verbose* T)
  (let ((concept (unknown-concept)))
    (networks-set-output *networks-set* *concepts-labels* (cdr concept))))

(defun networks-set (network-config concepts
		     activation-function learning-rates
		     threshold training-set testing-set quadratic-limit )
  ;;; generates a set of 10 perceptrons for the network
  (let* ((network-set ()))
    (dotimes (position (length concepts))
      (push (best-perceptron network-config concepts position activation-function
			     learning-rates threshold training-set
			     testing-set quadratic-limit)
	    network-set))
    (reverse network-set)))

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
      (cons (cons 1 (neuron inputs)) ;; adding a weight for the bias
	    (layer inputs (- outputs 1)))))

(defun neuron (inputs)
  ;;; inputs : the number of inputs (weights) for the neuron
  ;;; output : a list of weights (random) for each input
  (if (> inputs 0)
      (cons (random 1.0 )
	    (neuron (- inputs 1)))))

(defun best-perceptron (network-config concepts position activation-function learning-rates threshold
			training-set testing-set quadratic-limit)
  ;;; an exemple of what can be done with the perceptron
  (let ((best-network nil) (best-error-rate -1))
    (dotimes (x 1)
      (let* ((current-network (perceptron (network network-config)
					  concepts position activation-function
					  learning-rates threshold
					  training-set testing-set
					  quadratic-limit))
	     (recall (cdr (testing-perceptron current-network concepts position
					      threshold testing-set))))
	(when (> recall best-error-rate)
	  (setq best-error-rate recall)
	  (setq best-network current-network))))
    (perceptron best-network
		concepts position activation-function '(0.35) threshold
		(* training-set 0) testing-set quadratic-limit)
    best-network))

(defun perceptron (network concepts position activation-function learning-rates threshold
		   training-set testing-set &optional (quadratic-limit 0))
  ;;; inputs : configuration for a perceptron
  ;;; outputs : trained network
  (defparameter *activation-function* activation-function)
  (loop for learning-rate in learning-rates
     do (setq network (training-perceptron network
					   concepts
					   position
					   learning-rate
					   training-set
					   quadratic-limit))
       (when *verbose*
	 (format t "Error rate of ~a%~%"
		 (testing-perceptron network concepts position threshold testing-set))))
  network)

(defun training-perceptron (network concepts position learning-rate n quadratic-limit)
  ;;; inputs : a network and a concept, n the number of cycles
  ;;; runs n time the backtracking algorithm on a compatible representation of a
  ;;; representation of the concept AND a compatible representation of something that is
  ;;; not a representation of the concept, using new reprensations at each cycle.
  ;;; outputs : a trained network
  (defparameter *learning-rate* learning-rate)
  (with-open-file (file (format nil "lisp/perceptron/quadratic-error/~a-~a-~a.csv"
				(nth position concepts) n *learning-rate*)
			:direction :output :if-exists :overwrite :if-does-not-exist :create )
    (let ((quadratic-error-sum 0))
      (dotimes (x n)
	(let* ((random-concept (next-random-concept concepts position 0.5))
	       (network-output (network-output network (cdr random-concept)))
	       (network-error (network-error (car network-output)
					     (equal (nth position concepts)
						    (write-to-string (car random-concept)))))
	       (quadratic-error (quadratic-error (car network-output)
						 (equal (nth position concepts)
							(write-to-string (car random-concept)))))
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
	    (format t "~a reaching quadratic error after ~a iterations~%" (nth position concepts) x)
	    (return-from training-perceptron network))
	  (when *verbose*
	    (format t "Max iterations reached~%(sqrt MQE) : ~a~%~%"
		    (sqrt-mqe quadratic-error-sum n)))))))
  network)

(defun testing-networks-set (networks-set concepts n)
  ;; inputs : a trained network and a concept, n the number of tests to do
  ;; runs t time and compares the output of the network when submitted a compatible representation
  ;; of the concept and a valid representation of somethingg that is not a representation of the
  ;; concept
  ;; outputs : the error rate on the testing set
  (time (let ((total-success 0))
	  (dotimes  (position (length concepts))
	    (let ((success 0))
	      (dotimes (x n)
		(let* ((next-concept (next-random-concept concepts position 1 'testing))
		       (networks-set-output (networks-set-output networks-set
								 concepts
								 (cdr next-concept))))
		  (when (equal networks-set-output (write-to-string (car next-concept)))
		    (incf success)
		    (incf total-success))))
	      (format t "Concept: ~a~12t Error rate: ~a%~%"
		      (nth position concepts) (* (- 1 (/ success
							 n))
						 100.0))))
	  (format t "Global error rate: ~a%~%" (* (- 1 (/ total-success
							  (* n (length concepts))))
						  100.0)))))))
  concepts)

(defun testing-perceptron (network concepts position threshold n)
  ;;; inputs : a trained network and a concept, n the number of tests to do
  ;;; runs t time and compares the output of the network when submitted a compatible representation
  ;;; of the concept and a valid representation of somethingg that is not a representation of the
  ;;; concept
  ;;; outputs : the error rate on the testing set
  (let ((true-positive 0)
	(false-positive 0)
	(true-negative 0)
	(false-negative 0))
    (dotimes (x n)
      (let* ((next-concept (next-random-concept concepts position 0.5 'testing))
	     (network-output (car (network-output network (cdr next-concept)))))
	(if (car next-concept)
	    (if (> network-output threshold)
		(incf true-positive)
		(incf false-positive))
	    (if (< network-output threshold)
		(incf true-negative)
		(incf false-negative)))))
    (cons (precision true-positive false-positive)
	  (true-positive-rate true-positive false-negative))))

(defun precision (true-positive false-positive)
  (if (and (eq 0 true-positive) (eq 0 false-positive))
      0
      (/ true-positive (+ true-positive false-positive 0.0))))

(defun true-positive-rate (true-positive false-negative)
  ;;; also called recall
  (if (and (eq 0 false-negative) (eq 0 true-positive))
      0
      (/ true-positive (+ true-positive false-negative 0.0))))

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
  (push 1 input ) ;; bias
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

;;; To show the output of the networks-set for a given input (reload streams first) :
;;;(refresh-streams "8"
;;;(networks-set-output *networks-set* '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9") (cdr (next-concept T "8")))

;;; To see how ugly the images are :
;;; (loop for i from 1 to 10 do (display-image (cdr (next-concept T "8"))))
