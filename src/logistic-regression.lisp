(defpackage :cl-lr
  (:use :cl))
(in-package :cl-lr)

(defmacro with-find-max ((set-if-max &optional default-value) &body body)
  (let ((max (gensym)))
    `(let ((,max nil))
       (labels ((,set-if-max (x)
                  (when (or (null ,max) (< ,max x))
                    (setq ,max x))))
         ,@body)
       (or ,max ,default-value))))


(defun make-feature (index &optional weight)
  (assert (<= 0 index))
  (if (or (null weight) (= weight 1))
      index
      (cons index weight)))
(export 'make-feature)

(defun feature-index (feature)
  (if (consp feature) (car feature) feature))

(defun feature-weight (feature)
  (if (consp feature) (cdr feature) 1))


(defun make-feature-vector (features) features)
(export 'make-feature-vector)

(defun feature-vector-features (feature-vector) feature-vector)

(defun feature-vector-inner (feature-vector weight-array)
  (reduce #'+
   (mapcar (lambda (feature)
             (let ((index (feature-index feature))
                   (weight (feature-weight feature)))
               (* weight (aref weight-array index))))
           (feature-vector-features feature-vector))))
(export 'feature-vector-inner)


(defun make-empty-training-set ()
  (list nil))
(export 'make-empty-training-set)

(defun training-set-append! (training-set vectors gold-index)
  (push (cons vectors gold-index) (car training-set)))
(export 'training-set-append!)

(defun training-set-size (training-set)
  (length (car training-set)))

(defmacro do-training-set ((vectors gold-vector) training-set &body body)
  (let ((g (gensym)))
    `(dolist (,g (car ,training-set))
       (let ((,vectors (car ,g))
             (,gold-vector (nth (cdr ,g) (car ,g))))
         ,@body))))
(export 'do-training-set)


(defstruct (context (:constructor %make-context))
  constant dimension training-set training-set-size
  feature-index->gold-count)

(defun search-max-feature-index (training-set)
  (with-find-max (maximize! -1)
    (do-training-set (vectors gold-vector) training-set
      (declare (ignore gold-vector))
      (dolist (vector vectors)
        (dolist (feature (feature-vector-features vector))
          (maximize! (feature-index feature)))))))

(defun make-context (training-set)
  (let* ((dimension (1+ (search-max-feature-index training-set)))
         (weight-array (make-array dimension :initial-element 0.0d0)))
    (do-training-set (vectors gold-vector) training-set
      (declare (ignore vectors))
      (loop for feature in (feature-vector-features gold-vector)
            for index = (feature-index feature)
            for weight = (feature-weight feature)
            do (incf (aref weight-array index) weight)))
    (%make-context
     :constant (/ 1.0d0 (training-set-size training-set))
     :dimension dimension
     :training-set training-set
     :training-set-size (training-set-size training-set)
     :feature-index->gold-count weight-array)))


(defun compute-gradients (context weight-array)
  (let* ((dimension (context-dimension context))
         (training-set-size (context-training-set-size context))
         (obj-func 0.0d0)
         (gradients (make-array dimension :initial-element 0.0d0))
         (feature-scores (make-array dimension :initial-element 0.0d0)))
    (do-training-set (vectors gold-vector) (context-training-set context)
      (let ((scores (make-array (length vectors)))
            (sum 0.0d0))
        (loop for i from 0
              for vector in vectors do
          (let ((score (exp (feature-vector-inner vector weight-array))))
            (setf (aref scores i) score)
            (incf sum score)))

        (incf obj-func
              (+ (- (feature-vector-inner gold-vector weight-array))
                 (log sum)))

        (loop for i from 0
              for feature-vector in vectors do
          (progn
            (dolist (feature (feature-vector-features feature-vector))
              (incf (aref feature-scores (feature-index feature))
                    (/ (* (feature-weight feature) (aref scores i))
                       sum)))))))

    (setf obj-func (/ obj-func training-set-size))

    (let ((constant
           (context-constant context))
          (feature-index->gold-count
           (context-feature-index->gold-count context)))
      (dotimes (f dimension)
        (let ((%grad (- (aref feature-scores f)
                        (aref feature-index->gold-count f)))
              (w (aref weight-array f)))
          (setf (aref gradients f) (+ (/ %grad training-set-size)
                                      (* w constant)))
          ;; 1/2 * (||w||^2)
          (incf obj-func (/ (* w w constant) 2))))
      (values gradients obj-func))))


(defgeneric solve (solver dimension gradient-fn))
(export 'solve)

(defun fit (training-set solver)
  (setq training-set (list (reverse (car training-set))))
  (let ((context (make-context training-set)))
    (let ((gradient-fn (lambda (weight-array)
                         (compute-gradients context weight-array))))
      (solve solver (context-dimension context) gradient-fn))))
(export 'fit)
