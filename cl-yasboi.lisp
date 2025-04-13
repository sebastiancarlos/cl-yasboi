(defpackage #:cl-yasboi/cl-yasboi
  (:nicknames #:cl-yasboi) ; as this is the main package, I nickname it to the
                           ; main system name.
  (:use #:cl)
  (:import-from #:cl-yasboi/lib #:ayy #:lmao)
  (:import-from #:alexandria #:flatten)
  (:export #:ayy-lmao #:main))
(in-package #:cl-yasboi/cl-yasboi)

(defun ayy-lmao ()
  "Main function: Generates 'ayy' and 'lmao'"
  ; Note: This use of "alexandria:flatten" is for illustrative purposes of
  ; using an external library. Naturally, you wouldn't need to flatten a list
  ; that you are generating on the fly.
  (flatten (list (list (ayy)) (list (lmao)))))

(defun main ()
  "Executable entry point"
  ; Generate fabulous output: (ayy lmao)
  (format t "~A~%" (ayy-lmao)))
