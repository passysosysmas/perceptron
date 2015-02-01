This is the stub README.txt for the "perceptron" project.

Install quicklisp by typing this in your REPL :
(load "quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:add-to-init-file)

(optional) Add quicklisp-slime-helper by typing this in your REPL :
(ql:quickload "quicklisp-slime-helper")

Create a symbolic link between your project directory and the folder scanned by ASDF at startup :
ln -s ~/path/to/my/projects ~/.local/share/common-lisp/source/

Now in the REPL :

To load project type :
(ql:quickstart "percptron")

To load package type :
(in-package perceptron)




To format the train-labels and train-images

sed '1,10000!d' train-images > test-images
sed '10001,60000!d' train-images > train-images-2
sed '1,10000!d' train-labels > test-labels
sed '10001,60000!d' train-labels > train-labels-2

This separates the big files in 1 training set of 50000 images and 1 testing set of 10000 images

mv test-labels ~/lisp/perceptron/images/test/test-labels
mv test-images ~/lisp/perceptron/images/test/test-images
mv train-labels-2 ~/lisp/perceptron/images/train-labels
mv train-images-2 ~/lisp/perceptron/images/train-images

on utilise cette fonction modifiée pour générer les fichiers de test

(defun sort-images-by-labels (label-set)
    (mapcar #'(lambda (label)
	       (with-open-file (images (open "~/lisp/perceptron/images/test/test-images"))
		 (with-open-file (image-labels (open "~/lisp/perceptron/images/test/test-labels"))
		   (with-open-file (stream  (format nil "lisp/perceptron/images/test/~a" label)
					    :direction :output
					    :if-exists :overwrite
					    :if-does-not-exist :create )
		     (with-open-file (not-stream  (format nil "lisp/perceptron/images/test/not-~a" label)
						  :direction :output
						  :if-exists :overwrite
						  :if-does-not-exist :create )
		       (sort-images stream not-stream images image-labels label))))))
	    label-set))

