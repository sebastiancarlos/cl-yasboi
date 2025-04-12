(defpackage :cl-yasboi/test
  (:use :cl :fiveam)
  (:import-from :cl-yasboi/lib :ayy :lmao)
  (:import-from :cl-yasboi/cl-yasboi :ayy-lmao :main))
(in-package :cl-yasboi/test)

(def-suite cl-yasboi-test
   :description "Tests for cl-yasboi")

;;; Tests for cl-yasboi/lib
(def-suite* lib :in cl-yasboi-test
  :description "Tests for cl-yasboi/lib")

(test test-lib-ayy
  "Test the ayy function from the lib package"
  (is (string= "ayy" (ayy)))) 

(test test-lib-lmao
  "Test the lmao function from the lib package"
  (is (string= "lmao" (lmao))))

;;; Tests for cl-yasboi/cl-yasboi
(def-suite* main :in cl-yasboi-test
  :description "Tests for cl-yasboi/cl-yasboi")

(test test-main-ayy-lmao
  "Test the main ayy-lmao function"
  (is (equalp '("ayy" "lmao") (ayy-lmao))))

(test test-main-function-output
  "Test the output of the main function entry point"
  (is (string= (format nil "(ayy lmao)~%" (ayy-lmao))
               (with-output-to-string (*standard-output*) 
                 (main)))))
