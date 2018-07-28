(defpackage cl-lr.t.fiveam
  (:use :cl :fiveam))
(in-package :cl-lr.t.fiveam)
(def-suite :cl-lr)
(in-suite :cl-lr)

(test solve-with-liblbfgs
  (cl-lr.t.scenario.simple-calculation:solve-with-liblbfgs :test is))
