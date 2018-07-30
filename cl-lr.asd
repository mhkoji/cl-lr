(asdf:defsystem :cl-lr
  :serial t
  :pathname "src"
  :components
  ((:file "logistic-regression")

   (:module :solver
    :pathname "solver"
    :components
    ((:file "fn/gradient-descent")
     (:file "fn/liblbfgs/cffi")
     (:file "fn/liblbfgs/liblbfgs")
     (:file "solver"))))

  :depends-on (:cffi))

