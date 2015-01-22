;;;; perceptron.lisp
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
  ;;(let ((network (list (make-neuron-layer 784 hidden-neurons)
  ;;		       (make-neuron-layer hidden-neurons 10))))
  (let ((network (list(make-neuron-layer 784 hidden-neurons))))
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
  (map 'vector
       #'-
       output-values
       expected-values))
  
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
				(output-error output-values expected-values))))

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
  (* error (pdaf (weighted-sum neuron input-values))))
  
(defun pdaf (weighted-sum)
  ;; partial derivative of the activation function
  (let ((temp (activation-function weighted-sum)))
    (* temp (- 1 temp))))



(defun activation-function (weights inputs)
  (lambda (weights inputs) (+ (* (car weights)
				 (car inputs))
			      (lambda )
			      )
	  ))


(input (image ("tableau de 784 entiers de 0 à 255" (projet ("entier de 0 à 9")))))

(input (projet))

(defparameter *fichier-images* "adresse du fichier images")
(defparameter *fichier-labels* "adresse du fichier labels")
(defparameter *image* "tableau de 784 entiers de 0 à 255")

(defun images (fichier-images)
  '"liste d'images")

(lambda (fichier-images) 'image)

(defun projet-lisp (&optional input)
  ;; input : an image of 784 pixels
  ;; output : an integer between 0 and 9
  (if input
      'INPUT-ERROR
      (funcall (lambda () '"output"))))

;;;NEXT

(defun projet-lisp (&optional input)
  ;; input : an image of 784 pixels
  ;; output : an integer between 0 and 9  
  (check-type input (SIMPLE-VECTOR 784))
  'NOTIMPLEMENTED)


(defun image ()
  (let ((images (open "~/lisp/perceptron/train-images")))
    (let ((image (next-image images)))
      (close images)
      image)))

(defun next-image (images)
  (map 'vector
       #'(lambda (x) (/ (parse-integer x) 255.0))
       (split-sequence:split-sequence #\Space
				      (read-line images)
				      :remove-empty-subseqs t)))


;;;NEXT
(defun projet ()
  ;; input ??
  ;; output ??
  (let ((image (image)))
    (0-9 image)))

(defun projet ()
  ;; input : ?
  ;; output : ?
  (let (image-getter ()image-getter))
  (lambda (input)
    ;; input : an image of 784 pixels
    
    ;; output : an integer between 0 and 9  
    (check-type input (SIMPLE-VECTOR 784))
    'NOTIMPLEMENTED))

(defun projet ()
  (let ((images (open "~/lisp/perceptron/train-images")))
    (let ((image (next-image images)))
      (close images)
      (check-type image (SIMPLE-VECTOR 784))
      'NOTIMPLEMENTED
      )))

(defun next-image (images)
  (map 'vector
       #'(lambda (x) (/ (parse-integer x) 255.0))
       (split-sequence:split-sequence #\Space
				      (read-line images)
				      :remove-empty-subseqs t)))
(projet)


;;;NEXT


;;; opens the train-images file and store the stream in images
;;; images in the file are 60 000 lines of 784 values separated by spaces
;;; assign the evaluation of (next-image images) to image which takes the first image of the file
;;; and turn it into a vector of values between 0 and 1
;;; close the train-image file
(defun projet ()
  (let ((images (open "~/lisp/perceptron/train-images")))
    (let ((image (next-image images)))
      (check-type image (SIMPLE-VECTOR 784))
      (print 'NOT-IMPLEMENTED)
      (close images)
      )))

;;; need an open string with the right format
;;; next-image takes the stream of image and read a line
;;; parse the line for integers
;;; the current form only requires the file to be space separated values

;;; next-image need a stream from which it can use readline
;;; the resulting line must me composed of space separated integers of value between 0 and 255
;;; those integers are parsed and divided by integers
;;; the result is a vector of as many pixels as the line had values (784 in our exemple)
(defun next-image (images)
  (map 'vector
       #'(lambda (x) (/ (parse-integer x) 255.0))
       (split-sequence:split-sequence #\Space
				      (read-line images)
				      :remove-empty-subseqs t)))

;;; we need to change that behavior, this is really dirty and doesn't look like it has a good
;;; future. we need to be able to get the next images as well for future training and testing.

;;; next


(defun projet ()
  (with-open-file (images "~/lisp/perceptron/train-images")
    (let ((image (next-image images)))
      (check-type image (SIMPLE-VECTOR 784))
      'NOT-IMPLEMENTED)))


(defun next-image (images)
  (map 'vector
       #'(lambda (x) (/ (parse-integer x) 255.0))
       (split-sequence:split-sequence #\Space
				      (read-line images)
				      :remove-empty-subseqs t)))

;;; on voudrait pouvoir exectuer le test plusieurs fois


;; un perceptron ne devrait avoir qu'un flux continu d'images, il n'a qu'à appeler la fonction next pour passer à l'image suivante

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


(defun projet ()
  (let ((config-network '((784 #'logistic-function)
			  ;;(2 #'logistic-function)
			  )))
    (perceptron  config-network 0.1 0.5 0 1)))


(defun perceptron (neurons-set threshold learning-rate training-set testing-set)
  (defparameter *threshold* threshold)
  (defparameter *learning-rate* learning-rate)
  (with-open-file (images (open "~/lisp/perceptron/train-images"))
    (with-open-file (labels (open "~/lisp/perceptron/train-labels"))
      (let ((network (make-network neurons-set)))
	(print network)
	(print 'TRAINING)
	;;(time (setq network (training network images training-set)))
	(print 'TESTING)
	(time (print (error-rate (testing network images testing-set) testing-set)))))))


(defun training (network images n)  (print network)
  (dotimes (x n)
    (let ((inputs (next-image images)))
      (setq network (backtracking  network
				   inputs
				   (network-output network inputs)
				   (label-to-results (next-label *labels*)))))))
    

(defun testing (network images n)
  (print network)
  (let ((success 0))
    (dotimes (x n)
      (when (equal (results-to-labels (network-output network (next-image images)))
		   (next-label *labels*))
	(setq success (+ success 1))))
    success))


(defun error-rate (error-number testing-set)
  (if (> error-number 0)
      (- 100 (* (/ error-number testing-set) 100.0))
      NIL))


;;; TODO : make a network of functions and a network of weights
(defun make-network (neurons-set)
  ;; inpout : list of the number of inputs for each neuron layer
  ;; output : a list (neuron) network with (car neurons-set) inputs and 1 output
  (if (cdr neurons-set)
      (cons (make-neuron-layer (caar neurons-set) ; the inputs of the first layer
			       (caadr neurons-set) ; the outputs of the first layer
			       (cadar neurons-set)) ; the activation function of the first-layer
	    (make-network (cdr neurons-set)))
      (list (cadar neurons-set) ;; the activation function of the last layer
	    (make-neuron (caar neurons-set))))) ;; the inputs of the last layer


;; several inputs
;; one output
(defun make-neuron (inputs)
  (make-list inputs :initial-element 0))


;; several inputs
;; several outputs (1 per neuron)
(defun make-neuron-layer (inputs outputs activation-function)
  (list activation-function (make-list outputs :initial-element (make-neuron inputs))))


(defun network-output (network input-values)
  (if network
     (network-output (cdr network) (layer-output input-values
						  (cdr network) ;; layer
						  (cadar network))) ;; activation function
     ))  ;; activation-function


(defun layer-output (input-values layer activation-function)
  (mapcar (lambda (neuron) (neuron-output input-values neuron activation-function))
	  layer))  


(defun neuron-output (input-values weights activation-function)
  (funcall activation-function (weighted-sum weights input-values)))


(defun weighted-sum (weights input-values)
  (reduce '+ (map 'vector '* input-values weights)))


(defun logistic-function (weighted-sum)
  (/ 1 (+ 1 (exp (* -1 weighted-sum)))))


(defun output-error (output-values expected-values)
  (map 'vector
       #'-
       output-values
       expected-values))


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
				(output-error output-values expected-values))))

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
  (* error (pdaf (weighted-sum neuron input-values))))
  
(defun pdaf (weighted-sum)
  ;; partial derivative of the activation function
  (let ((temp (activation-function weighted-sum)))
    (* temp (- 1 temp))))
