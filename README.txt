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

