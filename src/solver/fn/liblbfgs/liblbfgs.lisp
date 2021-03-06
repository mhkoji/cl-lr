(defpackage :cl-lr.solver.fn.liblbfgs
  (:use :cl :cl-lr.solver.fn.liblbfgs.cffi)
  (:export :solve))
(in-package :cl-lr.solver.fn.liblbfgs)

(defvar *gradient-fn* nil)

(cffi:defcallback lbfgs-evaluate lbfgsfloatval-t
    ((instance  :pointer)
     (x         :pointer)
     (grad      :pointer)
     (n         :int)
     (step      lbfgsfloatval-t))
  (declare (ignore instance step))
  (let ((current-weights (make-array n :initial-element 0.0d0)))
    (dotimes (i n)
      (setf (aref current-weights i)
            (cffi:mem-aref x 'lbfgsfloatval-t i)))
    (multiple-value-bind (gradients obj-func)
        (funcall *gradient-fn* current-weights)
      (dotimes (i n)
        (setf (cffi:mem-aref grad 'lbfgsfloatval-t i)
              (aref gradients i)))
      obj-func)))

(cffi:defcallback lbfgs-progress :int
    ((instance  :pointer)
     (x         :pointer)
     (g         :pointer)
     (fx        lbfgsfloatval-t)
     (xnorm     lbfgsfloatval-t)
     (gnorm     lbfgsfloatval-t)
     (step      lbfgsfloatval-t)
     (n         :int)
     (k         :int)
     (ls        :int))
  (declare (ignore instance x g xnorm step n k ls))
  (format t "obj=~A, norm=~A~%" fx gnorm)
  0)

(defun solve (dimension gradient-fn &key (display-progress-p t))
  (let ((weights (make-array dimension :initial-element 0.0d0))
        (*gradient-fn* gradient-fn))
    (cffi:with-foreign-objects
        ((x     'lbfgsfloatval-t dimension)
         (param '(:struct lbfgs-parameter-t)))
      (dotimes (i dimension)
        (setf (cffi:mem-aref x 'lbfgsfloatval-t i) 0.0d0))
      (lbfgs-parameter-init param)
      (lbfgs dimension
             x
             (cffi:null-pointer)
             (cffi:callback lbfgs-evaluate)
             (if display-progress-p
                 (cffi:callback lbfgs-progress)
                 (cffi:null-pointer))
             (cffi:null-pointer)
             param)
      (dotimes (i dimension)
        (setf (aref weights i) (cffi:mem-aref x 'lbfgsfloatval-t i)))
      weights)))
