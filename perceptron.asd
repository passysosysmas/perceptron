;;;; perceptron.asd

(asdf:defsystem #:perceptron
  :description "Describe perceptron here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:split-sequence)
  :components ((:file "package")
               (:file "perceptron")
	       (:file "backtracking")
	       (:file "concepts")
	       (:file "image")))

