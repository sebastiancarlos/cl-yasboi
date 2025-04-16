; Loads the temporary local Quicklisp. Assumes './.ql-tmp/' was set up.
(load "./.ql-tmp/setup.lisp")

; Ensure ASDF finds the project in the current directory
(push *default-pathname-defaults* asdf:*central-registry*)

; loads the project system, and builds the executable.
(let ((sys (uiop:getenv "MAIN_SYSTEM_NAME")))
  (ql:quickload sys)
  (asdf:make sys))
