;; cl-yasboi.asd
;; Using ASDF's 'package-inferred-system'

; To make this local project accessible, make sure ASDF can find it.
; For example, if using ASDF's source registry, add the following:
;   (:directory "path/to/cl-yasboi")
; Then, fetch dependencies and run it with:
; * (ql:quickload "cl-yasboi")
; * (cl-yasboi:ayy-lmao) 
; ("ayy" "lmao")
;
; To run the tests, first fetch dependencies of the test system:
; * (ql:quickload "cl-yasboi/test")
; T
; * (asdf:test-system "cl-yasboi")
; Running test suite CL-YASBOI-TEST ...
; T

;; Main system
(defsystem "cl-yasboi"
  :class :package-inferred-system
  :description "cl-yasboi: Yet Another Starter Boilerplate"
  :version "1.0.0"
  :author "Sebastian Carlos"
  :license "MIT"
  :homepage "https://github.com/sebastiancarlos/cl-yasboi"
  :source-control "https://github.com/sebastiancarlos/cl-yasboi"
  ; Make it so an executable is build with "asdf:make"
  :build-operation program-op
  ; Entry point for the executable:
  :entry-point "cl-yasboi:main"
  ; Output path for the executable
  :build-pathname "cl-yasboi"
  ; While everything loads from the main package, it's good practice to list
  ; the external dependencies here too:
  :depends-on ("alexandria"
               ; The ".lisp" suffix is implicit. ASDF will interpret the
               ; following as the "./cl-yasboi.lisp" file.
               "cl-yasboi/cl-yasboi")
  ; Chain asdf:test-system to the test system:
  :in-order-to ((test-op (test-op "cl-yasboi/test"))))

;; Test system
(defsystem "cl-yasboi/test"
  :class :package-inferred-system
  :description "cl-yasboi's test suite"
  :depends-on ("fiveam")
  ; Show how to invoke FiveAM to run the test suite:
  :perform (test-op (o s) 
                    (uiop:symbol-call :fiveam '#:run! 
                       (uiop:find-symbol* '#:cl-yasboi-test :cl-yasboi/test))))
