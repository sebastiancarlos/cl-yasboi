; Loads the temporary local Quicklisp. Assumes './.ql-tmp/' was set up.
(load "./.ql-tmp/setup.lisp")

; Ensure ASDF finds the project in the current directory.
(push *default-pathname-defaults* asdf:*central-registry*)

; loads the test system, and runst the test suite.
(ql:quickload (uiop:getenv "TEST_SYSTEM_NAME"))
(asdf:test-system (uiop:getenv "MAIN_SYSTEM_NAME"))
