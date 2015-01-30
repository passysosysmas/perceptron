(in-package #:perceptron)

;;DONE
;;(sort-images-by-labels '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(defun sort-images-by-labels (label-set)
    (mapcar #'(lambda (label)
	       (with-open-file (images (open "~/lisp/perceptron/images/train-images"))
		 (with-open-file (image-labels (open "~/lisp/perceptron/images/train-labels"))
		   (with-open-file (stream  (format nil "lisp/perceptron/images/~a" label)
					    :direction :output
					    :if-exists :overwrite
					    :if-does-not-exist :create )
		     (with-open-file (not-stream  (format nil "lisp/perceptron/images/not-~a" label)
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
	(format temp image)
	(write-char #\linefeed temp))
      (sort-images stream not-stream images image-labels label))))
