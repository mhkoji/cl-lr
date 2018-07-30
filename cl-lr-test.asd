(asdf:defsystem :cl-lr-test
  :serial t
  :pathname "t"
  :components
  ((:module :scenario
    :pathname "scenario"
    :components
    ((:file "simple-calculation")))

   (:file "fiveam"))

  :perform (asdf:test-op (o s)
             (funcall (intern (symbol-name :run!) :fiveam) :cl-lr))

  :depends-on (:cl-lr :fiveam))
