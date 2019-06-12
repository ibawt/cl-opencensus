(in-package #:cl-opencensus)

(defclass attribute-map ()
  ((lock :accessor lock :initform (bt:make-lock))
   (head :accessor list-head :initform nil :type (or nil map-node))
   (tail :accessor list-tail :initform nil :type (or nil map-node))
   (dropped-count :initform 0)
   (count :initform 0 :type fixnum :accessor list-count)
   (max-count :reader max-count :initarg :max-count :type fixnum))
  (:documentation "A key value store with a capped number of entries, recent entries win.  Don't use for large max-counts"))
(export 'attribute-map)

(defmethod print-object ((a attribute-map) stream)
  (bt:with-lock-held ((lock a))
    (format stream "AttributeMap{ head: ~a, tail: ~a, count: ~a, max-count: ~a }"
            (list-head a)
            (list-tail a)
            (list-count a)
            (max-count a))))

(defmethod dropped-count ((a attribute-map))
  (bt:with-lock-held ((lock a))
      (slot-value a 'dropped-count)))

(defclass map-node ()
  ((value :accessor node-value :initarg :value)
   (key :reader node-key :initarg :key)
   (next :accessor node-next :initarg :next)
   (previous :accessor node-previous :initarg :previous)))

(defmethod print-object ((n map-node) stream)
  (format stream "[~a:~a]" (node-key n) (node-value n)))

(defmethod attribute-map-add ((d attribute-map) key value)
  (bt:with-lock-held ((lock d))
    (let ((existing (find-node d key value)))
     (if existing
         (progn
           (setf (node-value existing) value)
           (node-to-front d existing))
         (let ((n (make-instance 'map-node
                                 :value value
                                 :key key
                                 :next (list-head d)
                                 :previous nil)))
           (if (list-head d)
               (setf (node-previous (list-head d)) n))
           (unless (list-tail d)
             (setf (list-tail d) n))
           (setf (list-head d) n)
           (incf (list-count d))
           (when (> (list-count d) (max-count d) )
             (trim-tail d)))))))
(export 'attribute-map-add)

(defun attribute-map->list (d)
  (bt:with-lock-held ((lock d))
    (loop for x = (list-head d) then (node-next x)
         while x
         collect `(,(node-key x) ,(node-value x)))))
(export 'attribute-map->list)

(defun trim-tail (d)
  (when (list-tail d)
    (incf (slot-value d 'dropped-count)) 
    (setf (node-next (node-previous (list-tail d))) nil)
    (setf (list-tail d) (node-previous (list-tail d)))
    (decf (list-count d))))

(defun find-node (d key &optional (reverse nil))
  (loop for x =  (if reverse (list-tail d) (list-head d))
          then (if reverse (node-previous x) (node-next x))
        while x
        do (when (equal (node-key x) key)
             (return x))))

(defun remove-node (n)
  (when (node-previous n)
      (setf (node-next (node-previous n)) (node-next n)))
  (when (node-next n)
   (setf (node-previous (node-next n)) (node-previous n))))

(defun node-to-front (d n)
  (remove-node n)
  (setf (node-next n) (list-head d))
  (setf (node-previous n) nil)
  (setf (node-previous (list-head d)) n)
  (setf (list-head d) n))

