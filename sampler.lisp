(in-package #:cl-opencensus)

(defclass sampler (base) ())

(defclass sampling-parameters (base)
  ((parent-context :accessor parent-context :initarg :parent-context :type span-context)
   (trace-id :accessor trace-id :initarg :trace-id)
   (span-id :accessor span-id :initarg :span-id)
   (name :accessor name :initarg :name :type string)
   (has-remote-parent :accessor has-remote-parent :initarg :has-remote-parent)))

(defgeneric sample (sampler param)
  (:documentation "return one of :sample or :no-sample"))

(defclass never-sampler (sampler) ())

(defun never-sample ()
  (make-instance 'never-sampler))

(defmethod sample ((s never-sampler) param)
  (declare (ignore param))
  nil)

(defclass always-sampler (sampler) ())

(defun always-sample ()
  (make-instance 'always-sampler))

(defmethod sample ((s always-sampler) param)
  (declare (ignore param))
  t)

(defclass probability-sampler (sampler)
  ((trace-id-upper-bound :reader trace-id-upper-bound :initarg :trace-id-uppper-bound)))

(defmethod sample ((s probability-sampler) (p sampling-parameters))
  (if (sampled-p (parent-context s))
      t
      (let ((x (/ (intbytes:octets->uint64 (trace-id p)) 2)))
        (< x (trace-id-upper-bound s)))))

(defun probability-sampler (fraction)
  (if (>= fraction 1)
      (always-sample)
      (make-instance 'probability-sampler)) :trace-id-uppper-bound (floor (* fraction (expt 2 63))))
