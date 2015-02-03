() (declaim (optimize (speed 0) (safety 3) (debug 3)))
;; add evolutionary strategies for learning rate

;;;; perceptron.lisp
(in-package #:perceptron)

(defparameter *pathname* "lisp/perceptron/images/")
(defparameter *meta-perceptron* (make-hash-table :test 'equal))
(defparameter *verbose* T)

(defstruct (perceptron (:conc-name p-))
  network-config
  concept-label
  (network nil)
  (concepts '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (activation-function #'logistic-function)
  (learning-rate 0.1 )
  (momentum 0.1)
  (quadratic-limit 0.1)
  (threshold 0.8)
  (training-set 50000)
  (testing-set 100)
  (quadratic-evolution nil)
  (sqrt-mqe-evolution nil)
  (learning-rate-evolution nil)
  (true-positive 0)
  (true-negative 0)
  (false-positive 0)
  (false-negative 0))
	   
(defun bootstraping ()
  ;;; you need to put the image files into separated folders (see README.txt)
  (generate-files))

(defun main ()
  (let ((perceptron (make-perceptron :concept-label "0"
				     :network-config '(784))))
    (setf (p-network perceptron)
	  (network (p-network-config perceptron)))
    (open-streams (p-concepts perceptron))
    (format t "Network config: ~a~%Learning rate: ~a Momentum: ~a~%"
	    (p-network-config perceptron)
	    (p-learning-rate perceptron)
	    (p-momentum perceptron))
    (setf perceptron (training-perceptron perceptron))
    (testing-perceptron perceptron)
    ;;(print perceptron)
    ;;(format t "~a~%" (car (test-concept perceptron)))
    (perceptron-to-console perceptron)
    (push perceptron (gethash (p-concept-label perceptron) *meta-perceptron*))
    (statistics-perceptron perceptron)
    (save-perceptron perceptron)
    T
      ))

(defun test-concept (perceptron)
  (setf *verbose* T)
  (let ((concept (unknown-concept)))
    (display-image concept)
    (network-output (p-network perceptron) concept)))

;;; Constructors
(defun network (networks-config) 
  ;;; input : list of the number of inputs for each neuron layer
  ;;; output : a weights-network based on the input
  (if networks-config
      (cons (layer (first networks-config) ; the inputs of the current layer
		   (or (second networks-config) 1)) ; the outputs of the current layer
	    (network (rest networks-config))))) ; the inputs of the remaining layers

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

(defun best-perceptron (perceptron)
  ;;; an exemple of what can be done with the perceptron
  (let ((temp-perceptron perceptron)
	(best-error-rate -1))
    (dotimes (x 10)
      (setq temp-perceptron (make-perceptron
			     :concept-label (p-concept-label perceptron)
			     :network-config (p-network-config perceptron)))
      (setf (p-network temp-perceptron) (network (p-network-config perceptron)))

      (setq temp-perceptron (training-perceptron temp-perceptron))
      (let ((recall (true-positive-rate temp-perceptron)))
	(when (> recall best-error-rate)
	  (setq best-error-rate recall)
	  (setf perceptron temp-perceptron)))
      perceptron)))


(defun training-perceptron (perceptron)
;;; inputs : a network and a concept, n the number of cycles
;;; runs n time the backtracking algorithm on a compatible representation of a
;;; representation of the concept AND a compatible representation of something that is
;;; not a representation of the concept, using new reprensations at each cycle.
;;; outputs : a trained network
  (let* ((learning-rate (p-learning-rate perceptron))
	 (quadratic-error-sum 0)
	 (evolution-quadratic-error-sum 0)
	 (quadratic-evolution ())
	 (sqrt-mqe-evolution ())
	 (learning-rate-evolution ()))
    ;; most of the time is spent in this loop, increasing as with the number of iterataions
    ;; about 1 000 000 processor cycles are spent here
    (with-open-file (file (format nil "lisp/perceptron/quadratic-error/~{~a~^-~}/~a-~a-~a-~a.csv"
				  (p-network-config perceptron)
				  (p-concept-label perceptron)
				  learning-rate
				  (p-momentum perceptron)
				  (p-quadratic-limit perceptron))
			  :direction :output :if-exists :overwrite :if-does-not-exist :create )
      (dotimes (x (p-training-set perceptron))
	(let* ((random-concept (next-random-concept (p-concept-label perceptron)
						    (p-concepts perceptron) 0.5))
	       (network-output (network-output (p-network perceptron) (cdr random-concept)))
	       (network-error (network-error (car network-output)
					     (equal (p-concept-label perceptron)
						    (write-to-string (car random-concept)))))
	       (quadratic-error (quadratic-error (car network-output)
						 (equal (p-concept-label perceptron)
							(write-to-string (car random-concept)))))
	       (sqrt-mqe (sqrt-mqe quadratic-error-sum x)))
					;(print network-output)
	  (incf quadratic-error-sum quadratic-error)
	  (incf evolution-quadratic-error-sum quadratic-error)
	  ;; save medium QE to a file
	  ;; about 16000, but writing in a file, might be a bottleneck
	  (format file "~a,~a~%" sqrt-mqe network-error)
	  (push quadratic-error quadratic-evolution)
	  (push sqrt-mqe sqrt-mqe-evolution)
	  (push learning-rate learning-rate-evolution)


	  ;; start backtracking
	  (setf (p-network perceptron)
		(backtracking perceptron
			      (append (list (cdr random-concept))
				      (cdr network-output))
			      network-error))
	  (when (and (> x 100)(> (p-quadratic-limit perceptron) sqrt-mqe))
	    (format t "Concept ~a reaching quadratic limit of ~a after ~a iterations~%"
		    (p-concept-label perceptron)
		    (p-quadratic-limit perceptron)
		    x)
	    (return-from training-perceptron perceptron))
	  (when (eq 0 (mod x 100))
	    (if (> (/ evolution-quadratic-error-sum 100)
		   sqrt-mqe)
		(incf learning-rate (/ (p-momentum perceptron) 100))
		(decf learning-rate (/ (p-momentum perceptron) 100)))
	    (setf evolution-quadratic-error-sum 0)))))
    
    (setf (p-quadratic-evolution perceptron) quadratic-evolution)
    (setf (p-sqrt-mqe-evolution perceptron) sqrt-mqe-evolution)
    (setf (p-learning-rate-evolution perceptron) learning-rate-evolution)
    
    (when *verbose*
      (format t "Concept ~a reaching iteration limit ~%Sqrt MQE : ~a~%~%"
	      (p-concept-label perceptron)
	      (sqrt-mqe quadratic-error-sum (p-training-set perceptron))))
    perceptron))


(defun testing-networks-set (networks-set concepts n)
  ;; inputs : a trained network and a concept, n the number of tests to do
  ;; runs t time and compares the output of the network when submitted a compatible representation
  ;; of the concept and a valid representation of somethingg that is not a representation of the
  ;; concept
  ;; outputs : the error rate on the testing set
  (let ((total-success 0))
    (dotimes  (position (length concepts))
      (let ((success 0))
	(loop for concept in concepts
	     do (let* ((next-concept (next-random-concept concept
						    concepts
						    1 'testing))
		 (networks-set-output (networks-set-output networks-set
							   concepts
							   (cdr next-concept))))
	    (when (equal networks-set-output (write-to-string (car next-concept)))
	      (incf success)
	      (incf total-success)))
	     (format t "Concept: ~a~12t Error rate: ~a%~%"
		     concept (* (- 1 (/ success
					n))
				100.0)))))
    (format t "Global error rate: ~a%~%" (* (- 1 (/ total-success
						    (* n
						       (length concepts))))
					    100.0))))

(defun testing-perceptron (perceptron)
  ;;; inputs : a trained network and a concept, n the number of tests to do
  ;;; runs t time and compares the output of the network when submitted a compatible representation
  ;;; of the concept and a valid representation of somethingg that is not a representation of the
  ;;; concept
  ;;; outputs : the error rate on the testing set
  (dotimes (x (p-testing-set perceptron))
    (let* ((next-concept (next-random-concept (p-concept-label perceptron)
					      (p-concepts perceptron)
					      0.5 'testing))
	   (network-output (car (network-output (p-network perceptron) (cdr next-concept)))))
      (if (eq (p-concept-label perceptron) (car next-concept))
	  (if (> network-output (p-threshold perceptron))
	      (incf (p-true-positive perceptron))
	      (incf (p-false-negative perceptron)))
	  (if (< network-output (p-threshold perceptron))
	      (incf (p-true-negative perceptron))
	      (incf (p-false-positive perceptron))))
      perceptron)))

(defun statistics-perceptron (perceptron)
  (format t "True postive: ~a    False positive: ~a~%True negative:  ~a   False negative: ~a~%Precision : ~a%   Recall : ~a%~%"
	  (p-true-positive perceptron)
	  (p-false-positive perceptron)
	  (p-true-negative perceptron)
	  (p-false-negative perceptron)
	  (precision perceptron)
	  (true-positive-rate perceptron)))

(defun precision (perceptron)
  (if (and (eq 0 (p-true-positive perceptron)) (eq 0 (p-false-positive perceptron)))
      0
      (* 100 (/ (p-true-positive perceptron)
		(+ (p-true-positive perceptron) (p-false-positive perceptron) 0.0)))))

(defun true-positive-rate (perceptron)
  ;;; also called recall
  (if (and (eq 0 (p-false-negative perceptron)) (eq 0 (p-true-positive perceptron)))
      0
      (* 100 (/ (p-true-positive perceptron)
		(+ (p-true-positive perceptron) (p-false-negative perceptron) 0.0)))))

(defun networks-set-output (networks-set concepts input &optional detailed)
()  (when *verbose* (display-image input))
  (let ((outputs (mapcar (lambda (output concept)
			   (when *verbose*
			     (format t "Concept ~a : ~a%~%" concept
						   (if (> output 0.0001) (* 100 output) "< 0.01")))
			   output)
		 (mapcar (lambda (network)
			   (car (network-output network input)))
			 networks-set)
		 concepts)))
    (if detailed
	outputs
	(nth (position (reduce #'max outputs) outputs) concepts))))

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
  (logistic-function (weighted-sum neuron input)))


(defun weighted-sum (neuron input)
  ;;; input a list of weights and valid input
  ;;; hadoop or whatever distribited magic should be used here and there for the rise of Skynet
  ;;; output : the weighted sum
  (push 1 input ) ;; bias
  (+ (reduce #'+ (mapcar #'* neuron input))))

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
