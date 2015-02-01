;;;; perceptron.asd

(asdf:defsystem #:perceptron
  :description "This package allows building of perceptrons that are able to learn and recognize concepts"
  :author "Your Name <yenda1@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:split-sequence)
  :components ((:file "package")
               (:file "perceptron")
	       (:file "backtracking")
	       (:file "concepts")
	       (:file "image")))

