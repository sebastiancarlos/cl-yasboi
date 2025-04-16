; Installs Quicklisp into a temporary local ".ql-tmp/" directory. 
; Assumes the 'quicklisp.lisp' bootstrap file exists in the current dir.
(load "quicklisp.lisp")
(quicklisp-quickstart:install :path "./.ql-tmp/")
