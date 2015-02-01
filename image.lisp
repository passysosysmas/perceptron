(in-package #:perceptron)

;;;; Use the following commands to generate the test files (carefull with pathnames,
;;;; you might want to change them


(defun generate-files ()
  (sort-images-by-labels '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (sort-test-images-by-labels '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

(defun sort-test-images-by-labels (label-set)
    (mapcar #'(lambda (label)
	       (with-open-file (images (open "~/lisp/perceptron/images/test/test-images"))
		 (with-open-file (image-labels (open "~/lisp/perceptron/images/test/test-labels"))
		   (with-open-file (stream  (format nil "lisp/perceptron/images/test/~a" label)
					    :element-type '(unsigned-byte 8)
					    :direction :output
					    :if-exists :overwrite
					    :if-does-not-exist :create )
		     (with-open-file (not-stream  (format nil "lisp/perceptron/images/test/not-~a" label)
						  :element-type '(unsigned-byte 8)
						  :direction :output
						  :if-exists :overwrite
						  :if-does-not-exist :create )
		       (sort-images stream not-stream images image-labels label))))))
	    label-set))

(defun sort-images-by-labels (label-set)
    (mapcar #'(lambda (label)
	       (with-open-file (images (open "~/lisp/perceptron/images/train-images"))
		 (with-open-file (image-labels (open "~/lisp/perceptron/images/train-labels"))
		   (with-open-file (stream  (format nil "lisp/perceptron/images/~a" label)
					    :element-type '(unsigned-byte 8)
					    :direction :output
					    :if-exists :overwrite
					    :if-does-not-exist :create )
		     (with-open-file (not-stream  (format nil "lisp/perceptron/images/not-~a" label)
						  :element-type '(unsigned-byte 8)
						  :direction :output
						  :if-exists :overwrite
						  :if-does-not-exist :create )
		       (sort-images stream not-stream images image-labels label))))))
	    label-set))

(defun sort-images (stream not-stream images image-labels label)
  (let ((image (read-line images nil))
	(image-label (read-line image-labels nil)))
    (when (and image image-label)
      (let ((temp (if (equal image-label label) stream not-stream)))
	(write-sequence (next-image-extraction image) temp))
      (sort-images stream not-stream images image-labels label))))

(defun next-image-extraction (image)
  ;;; 
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

(defun network-to-console ()
  ;;; print the trained network in *standard-output*
  (let ((image (cdaar (cadr *networks-set*))))
    (display-image image))))

(defun network-to-file ()
  ;;; copy the trained network into a js file (in json format)
  (with-open-file (stream "lisp/perceptron/images/perceptron.js"
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
