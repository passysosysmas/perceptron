(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:perceptron)

;;;; This file contains the functions related to the generation of binary image files
;;;; for the perceptron to process
;;;; There is also few functions to display various data related to the images/network

;;;; TODO : pass labels and pathname as a parameter

(defun generate-files ()
  ;;; Use the following commands to generate the test files (carefull with pathnames,
  ;;; you might want to change them
  (sort-images-by-labels '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (sort-images-by-labels '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9") T)
  (unlabeled-to-binary))

(defun unlabeled-to-binary ()
  ;; this generates a binary file with the train-image file (bitmaps without labels)
  (with-open-file (images (open "~/lisp/perceptron/test-images"))
    (with-open-file (binary (open "~/lisp/perceptron/test-images-binary")
			    :element-type '(unsigned-byte 8)
			    :direction :output
			    :if-exists :overwrite
			    :if-does-not-exist :create )
      (dotimes (x 10000)
	(write-sequence (next-image-extraction (read-line images nil)) binary)))))

(defun sort-images-by-labels (label-set &optional testing)
  (mapcar #'(lambda (label)
	      (with-open-file (images (if testing
					  (open "~/lisp/perceptron/images/test/test-images")
					  (open "~/lisp/perceptron/images/train-images")))
		(with-open-file (image-labels (if testing
						  (open "~/lisp/perceptron/images/test/test-labels")
						  (open "~/lisp/perceptron/images/train-labels")))
		  (with-open-file (stream  (format nil "lisp/perceptron/images/~a~a"
						   (if testing "test/" "") label)
					   :element-type '(unsigned-byte 8)
					   :direction :output
					   :if-exists :overwrite
					   :if-does-not-exist :create )
		    (sort-images stream images image-labels label)))))
	  label-set)))

(defun sort-images (stream images image-labels label)
  ;;; 
  (let ((image (read-line images nil))
	(image-label (read-line image-labels nil)))
    (when (and image image-label)
      (when (equal image-label label)
	(write-sequence (next-image-extraction image) stream))
      (sort-images stream images image-labels label))))

(defun next-image-extraction (image)
  ;;; extracts the bitmap from the string and returns a list of integers
  (loop :for (integer position) := (multiple-value-list 
				    (parse-integer image
                                                   :start (or position 0)
                                                   :junk-allowed t))
     :while integer
     :collect integer))


(defun display-image (image)
  ;;; print the given image in *standard-output*
  (let ((x 0))
    (loop for pixel in image
       do (incf x)
	 (format t "~a" (if (< 0.01 pixel) "." "#"))
	 (when (eq 0 (mod x 28))
	   (format t "~%")))))

(defun display-average-image (concept)
  ;;; print the average form of 100 images of the same concept
  ;;; basically, it prints a potatoe
  (refresh-streams concept)
  (let ((average-concept (make-sequence 'list 784 :initial-element 0)))
    (dotimes (x 100)
      (setq average-concept (mapcar #'+
				    average-concept
				    (cdr (next-concept T concept)))))
    (display-image (mapcar (lambda (pixel) (/ pixel 100))
			      average-concept))))

(defun network-to-console (position)
  ;;; print the trained network in *standard-output*
  (display-image (cdaar (nth position *networks-set*))))

(defun network-to-file ()
  ;;; copy the trained network into a js file (in json format)
  (with-open-file (stream "lisp/perceptron/perceptron.js"
			  :direction :output
			  :if-exists :overwrite
			  :if-does-not-exist :create )
    (format stream "var perceptron = ")
    (write-char #\{ stream)
    (mapcar (lambda (perceptron label)
	      (format stream "\"~a\": [~{~a~^, ~}], " label (cdaar perceptron)))
	    *networks-set*
	    '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
    (write-char #\} stream)))
