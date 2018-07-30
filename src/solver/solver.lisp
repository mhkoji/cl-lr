(defpackage :cl-lr.solver
  (:use :cl)
  (:export :liblbfgs :gradient-descent))
(in-package :cl-lr.solver)

(defclass liblbfgs () ())

(defmethod cl-lr:solve ((solver liblbfgs) dimension gradient-fn)
  (cl-lr.solver.fn.liblbfgs:solve dimension gradient-fn))


(defclass gradient-descent () ())

(defmethod cl-lr:solve ((solver gradient-descent) dimension gradient-fn)
  (cl-lr.solver.fn.gradient-descent:solve dimension gradient-fn))
