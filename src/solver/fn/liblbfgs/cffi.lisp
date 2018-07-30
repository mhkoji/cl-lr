(defpackage :cl-lr.solver.fn.liblbfgs.cffi
  (:use :cl)
  (:export :lbfgsfloatval-t
           :lbfgs-parameter-t
           :lbfgs-parameter-init
           :lbfgs))
(in-package :cl-lr.solver.fn.liblbfgs.cffi)

(cffi:define-foreign-library liblbfgs
  (:unix "liblbfgs.so"))

(cffi:use-foreign-library liblbfgs)

;; Assume that lbfgsfloatval_t is defined as double in lbfgs.h
(cffi:defctype lbfgsfloatval-t :double)

(cffi:defcstruct lbfgs-parameter-t
  (m                  :int)
  (epsilon            lbfgsfloatval-t)
  (past               :int)
  (delta              lbfgsfloatval-t)
  (max-iterations     :int)
  (linesearch         :int)
  (max-linesearch     :int)
  (min-step           lbfgsfloatval-t)
  (max-step           lbfgsfloatval-t)
  (ftol               lbfgsfloatval-t)
  (wolfe              lbfgsfloatval-t)
  (gtol               lbfgsfloatval-t)
  (xtol               lbfgsfloatval-t)
  (orthantwise-c      lbfgsfloatval-t)
  (orthantwise-start  :int)
  (orthantwise-end    :int))


(cffi:defcfun "lbfgs_parameter_init" :void
  (param (:pointer (:struct lbfgs-parameter-t))))


(cffi:defcfun "lbfgs" :int
  (n             :int)
  (x             :pointer)
  (ptr_fx        :pointer)
  (proc_evaluate :pointer)
  (proc_progress :pointer)
  (instance      :pointer)
  (param         (:pointer (:struct lbfgs-parameter-t))))
