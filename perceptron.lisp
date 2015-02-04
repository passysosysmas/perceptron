(declaim (optimize (speed 0) (safety 3) (debug 3)))
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
  (learning-rate 0.5)
  (momentum 0.01)
  (quadratic-limit 0.2)
  (threshold 0.8)
  (training-set 50000)
  (testing-set 1000)
  (quadratic-evolution nil)
  (sqrt-mqe-evolution nil)
  (learning-rate-evolution nil)
  (true-positive 0)
  (true-negative 0)
  (false-positive 0)
  (false-negative 0)
  (false-positive-concepts ())
  (false-negative-concepts ()))
	   
(defun bootstraping ()
  ;;; You need to put the image files into separated folders (see README.txt)
  (generate-files))

(defun learn-concepts (concepts)
  ;;; This is the function to use if you want to learn multiple concepts at once
  (loop for concept in concepts
     do (learn concept '(784 3) 10 'precision)))

(defun learn (concept network-config generations &optional mode)
  ;;; This is the function to use you want to learn a single concept
  (let ((perceptron (make-perceptron :concept-label concept
				     :network-config network-config)))
    (setf (p-network perceptron)
	  (network (p-network-config perceptron)))
    (open-streams (p-concepts perceptron))
    (format t "Network config: ~a~%Learning rate: ~a Momentum: ~a~%"
	    (p-network-config perceptron)
	    (p-learning-rate perceptron)
	    (p-momentum perceptron))
    (setf perceptron (best-perceptron perceptron generations mode))
    (perceptron-to-console perceptron)
    (statistics-perceptron perceptron)
    (let ((best-perceptron (gethash (p-concept-label perceptron) *meta-perceptron*)))
      (when (> (F1 perceptron) (if best-perceptron (F1 best-perceptron) 0))
	(setf (gethash (p-concept-label perceptron) *meta-perceptron*) perceptron)))
    T))

(defun compare-concepts (concept1 concept2 network-config generations)
  ;;; This is the function to you when you want to make a perceptron specialized in the distinction
  ;;; of two concepts
  (let ((perceptron (make-perceptron :concept-label concept1
				     :concepts (list concept1 concept2)
				     :network-config network-config
				     :threshold 0.9)))
    (setf (p-network perceptron)
	  (network (p-network-config perceptron)))
    (open-streams (p-concepts perceptron))
    (format t "Network config: ~a~%Learning rate: ~a Momentum: ~a~%"
	    (p-network-config perceptron)
	    (p-learning-rate perceptron)
	    (p-momentum perceptron))
    (setf perceptron (best-perceptron perceptron generations 'review))
    (perceptron-to-console perceptron)
    (statistics-perceptron perceptron)
    (save-perceptron perceptron)
    T))

(defun test-concept ()
  (setf *verbose* T)
  (let ((concept (unknown-concept)))
    (display-image concept)
    (meta-perceptron-output concept)))

(defun testing-meta-perceptron (&optional (concepts '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
  (let ((global-count 0))
    (loop for concept in concepts
       do (let ((count 0))
	    (dotimes (x 10000)
	      (when (equal concept
			   (car (meta-perceptron-output (cdr (next-concept concept 'testing)))))
		(incf count)
		(incf global-count)))
	    (format t "10000 representation of ~a submitted with ~$% recognition rate~%"
		    concept
		    (/ count 100))))
    (format t "Average success rate for all concepts: ~$%" (/ global-count (* 100
									      (length concepts))))))

(defun meta-perceptron-output (input)
  (let ((max-label nil)
	(max 0))
    (maphash (lambda (label perceptron)
	       (let ((result (car (network-output (p-network perceptron) input)))) 
		 (when (> result max)
		   (setf max-label label
			 max result))))
	     *meta-perceptron*)
    (cons max-label max)))

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

(defun best-perceptron (perceptron generations mode)
  ;;; an exemple of what can be done with the perceptron
  (let ((temp-perceptron perceptron)
	(best-error-rate -1))
    (dotimes (x generations)
      (setq temp-perceptron (make-perceptron
			     :concept-label (p-concept-label perceptron)
			     :concepts (p-concepts perceptron)
			     :threshold (p-threshold perceptron)
			     :network-config (p-network-config perceptron)))
      (setf (p-network temp-perceptron) (network (p-network-config perceptron)))
      (setf temp-perceptron (training-perceptron temp-perceptron))
      (setf temp-perceptron (testing-perceptron temp-perceptron))
      (let ((recall (true-positive-rate temp-perceptron)))
	(when (> recall best-error-rate)
	  (setf best-error-rate recall)
	  (setf perceptron temp-perceptron)))))
  (format t "Best perceptron selected :~%")
  (statistics-perceptron perceptron)
  (when mode
    (format t "~%Starting extra-training to enhance ~a:~%"
	    mode)
    (setf (p-true-positive perceptron) 0
	  (p-false-negative perceptron) 0
	  (p-true-negative perceptron) 0
	  (p-false-positive perceptron) 0
	  perceptron (testing-perceptron (training-perceptron perceptron mode))))
  perceptron)


(defun training-perceptron (perceptron &optional mode)
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
    (dotimes (x (p-training-set perceptron))
      (let* ((next-concept (next-random-concept perceptron 0.5 mode))
	     (network-output (network-output (p-network perceptron) (cdr next-concept)))
	     (network-error (network-error (car network-output)
					   (equal (p-concept-label perceptron)
						  (car next-concept))))
	     (quadratic-error (quadratic-error (car network-output)
					       (equal (p-concept-label perceptron)
						      (write-to-string (car next-concept)))))
	     (sqrt-mqe (sqrt-mqe quadratic-error-sum x)))
	;; updating statistics
	(incf quadratic-error-sum quadratic-error)
	(incf evolution-quadratic-error-sum quadratic-error)
	;; collecting statistics
	(push quadratic-error quadratic-evolution)
	(push sqrt-mqe sqrt-mqe-evolution)
	(push learning-rate learning-rate-evolution)
	;; starting backtracking
	(setf (p-network perceptron)
	      (backtracking perceptron
			    (append (list (cdr next-concept))
				    (cdr network-output))
			    network-error))
	(when mode
	  (if (equal (p-concept-label perceptron) (car next-concept))
	      (when (and (member mode '(review recall))
			 (< (car network-output) (p-threshold perceptron)))
		(push next-concept (p-false-negative-concepts perceptron)))
	      (when (and (member mode '(review precision))
			 (> (car network-output) (p-threshold perceptron)))
		(push next-concept (p-false-positive-concepts perceptron)))))
	
	(when (and (> x 100)(> (p-quadratic-limit perceptron) sqrt-mqe))
  	  (format t "Concept ~a reaching quadratic limit of ~a after ~a iterations~%"
		  (p-concept-label perceptron)
		  (p-quadratic-limit perceptron)
		  x)
	  (nconc (p-quadratic-evolution perceptron) (average-stats quadratic-evolution x))
	  (nconc (p-sqrt-mqe-evolution perceptron) (average-stats sqrt-mqe-evolution x))
	  (nconc (p-learning-rate-evolution perceptron) (average-stats learning-rate-evolution x))
	  (return-from training-perceptron perceptron))
	(when (eq 0 (mod x 1))
	  (if (> (/ evolution-quadratic-error-sum 100)
		 sqrt-mqe)
	      (incf learning-rate (/ (p-momentum perceptron) 100))
	      (decf learning-rate (/ (p-momentum perceptron) 100)))
	  (setf evolution-quadratic-error-sum 0))))
    (nconc (p-quadratic-evolution perceptron) (average-stats quadratic-evolution
							    (p-training-set perceptron)))
    (nconc (p-sqrt-mqe-evolution perceptron) (average-stats sqrt-mqe-evolution
							    (p-training-set perceptron)))
    (nconc (p-learning-rate-evolution perceptron) (average-stats learning-rate-evolution
								 (p-training-set perceptron)))
     
    (when *verbose*
      (format t "Concept ~a reaching iteration limit ~%Sqrt MQE : ~a~%~%"
	      (p-concept-label perceptron)
	      (sqrt-mqe quadratic-error-sum (p-training-set perceptron)))))
  perceptron)

(defun average-stats (points nb-points)
  (let ((stepping (/ nb-points 1000))
	(sum 0)
	(new-points ()))
    (dotimes (x nb-points)
      (incf sum (pop points))
      (when (eq 0 (mod x stepping))
	(push (/ sum stepping) new-points)))
    new-points))

(defun testing-perceptron (perceptron)
  ;;; inputs : a trained network and a concept, n the number of tests to do
  ;;; runs t time and compares the output of the network when submitted a compatible representation
  ;;; of the concept and a valid representation of somethingg that is not a representation of the
  ;;; concept
  ;;; outputs : the error rate on the testing set
  (dotimes (x (p-testing-set perceptron))
    (let* ((next-concept (next-random-concept perceptron 0.5 'testing))
	   (network-output (car (network-output (p-network perceptron) (cdr next-concept)))))
      (if (equal (p-concept-label perceptron) (car next-concept))
	  (if (> network-output (p-threshold perceptron))
	      (incf (p-true-positive perceptron))
	      (incf (p-false-negative perceptron)))
	  (if (< network-output (p-threshold perceptron))
	      (incf (p-true-negative perceptron))
	      (incf (p-false-positive perceptron))))))
  perceptron)

(defun statistics-perceptron (perceptron)
  (format t "True postive: ~a    False positive: ~a~%True negative:  ~a   False negative: ~a~%Precision : ~a%   Recall : ~a%    F1 : ~a%~%"
	  (p-true-positive perceptron)
	  (p-false-positive perceptron)
	  (p-true-negative perceptron)
	  (p-false-negative perceptron)
	  (precision perceptron)
	  (true-positive-rate perceptron)
	  (F1 perceptron)))

(defun F1 (perceptron)
  (* 100 (/ (* 2 (p-true-positive perceptron))
	    (+ (* 2.0 (p-true-positive perceptron))
	       (p-false-positive perceptron) (p-false-negative perceptron)))))

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
