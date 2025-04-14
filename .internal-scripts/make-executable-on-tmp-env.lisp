;;; Loads a temporary local Quicklisp, loads the project system, and builds the executable.
;;; Assumes it is run from the project root directory and './.ql-tmp/' was already set up.

(load "./.ql-tmp/setup.lisp")

; Ensure ASDF finds the project in the current directory
(push *default-pathname-defaults* asdf:*central-registry*)

(ql:quickload "cl-yasboi")
(asdf:make "cl-yasboi")
