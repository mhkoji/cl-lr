(defpackage :cl-lr.t.scenario.simple-calculation
  (:use :cl)
  (:export :solve-with-liblbfgs))
(in-package :cl-lr.t.scenario.simple-calculation)

(defmacro solve-with-liblbfgs (&key test)
  `(let ((training-set (cl-lr:make-empty-training-set)))
     (cl-lr:training-set-append! training-set
      (list (cl-lr:make-feature-vector
             (list (cl-lr:make-feature 0)
                   (cl-lr:make-feature 1)
                   (cl-lr:make-feature 2)))
            (cl-lr:make-feature-vector
             (list (cl-lr:make-feature 3)
                   (cl-lr:make-feature 4)
                   (cl-lr:make-feature 5))))
      0)
     (cl-lr:training-set-append! training-set
      (list (cl-lr:make-feature-vector
             (list (cl-lr:make-feature 6)
                   (cl-lr:make-feature 7)
                   (cl-lr:make-feature 8)))
            (cl-lr:make-feature-vector
             (list (cl-lr:make-feature 9)
                   (cl-lr:make-feature 10)
                   (cl-lr:make-feature 11))))
      1)
     (let ((solver (make-instance 'cl-lr.solver:liblbfgs)))
       (let ((weight-vector-actual
              (cl-lr:fit training-set solver))
             (weight-vector-expected
              #(0.21542327109008128d0 0.21542327109008128d0
                0.21542327109008128d0 -0.2154232710900813d0
                -0.2154232710900813d0 -0.2154232710900813d0
                -0.2154232710900813d0 -0.2154232710900813d0
                -0.2154232710900813d0 0.21542327109008128d0
                0.21542327109008128d0 0.21542327109008128d0)))
         (,test (= (length weight-vector-actual)
                   (length weight-vector-expected)))
         (loop for a across weight-vector-actual
               for e across weight-vector-expected
               do (,test (= a e)))))))
