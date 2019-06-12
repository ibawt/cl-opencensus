(in-package #:cl-opencensus)

(defclass base ()
  ()
  (:documentation "base opencensus class for everything"))

(defclass sampler (base) ()
  (:documentation "base of all samplers"))

(defgeneric sample (sampler param)
  (:documentation "returns a sampling choice given the parameterss"))

(defclass never-sampler (sampler) ())

(defun never-sample ()
  "returns a never-sampler instance"
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

(defclass sampling-parameters (base)
  ((parent-context :accessor parent-context :initarg :parent-context :type span-context)
   (trace-id :accessor trace-id :initarg :trace-id)
   (span-id :accessor span-id :initarg :span-id)
   (name :accessor name :initarg :name :type string)
   (has-remote-parent :accessor has-remote-parent :initarg :has-remote-parent)))

(defmethod sample ((s probability-sampler) (p sampling-parameters))
  (if (sampled-p (parent-context s))
      t
      (let ((x (/ (intbytes:octets->uint64 (trace-id p)) 2)))
        (< x (trace-id-upper-bound s)))))

(defun probability-sampler (fraction)
  "creates a sampler with probably of the decimal fraction. 0 <= fraction < 1"
  (if (>= fraction 1)
      (always-sample)
      (make-instance 'probability-sampler)) :trace-id-uppper-bound (floor (* fraction (expt 2 63))))

(defclass config (base)
  ((lock :accessor lock :initform (bt:make-lock))
   (default-sampler :accessor default-sampler :initform (always-sample))
   (max-annotation-events-per-span :accessor max-annotations-events-per-span :initform 32)
   (max-message-events-per-span :accessor max-message-events-per-span :initform 128)
   (max-attributes-per-span :accessor max-attributes-per-span :initform 32)
   (max-links-per-span :accessor max-links-per-span :initform 32))
  (:documentation "configuration for cl-opencensus"))
(defvar *config* (make-instance 'config))

(defclass span (base)
  ((data :initform nil :initarg :data :accessor span-data :type (or nil span-data))
   (lock :initform (bt:make-lock) :accessor span-lock)
   (exported :initform nil :accessor span-exported)
   (span-context :accessor span-context :initform nil)
   (attributes :accessor attributes)
   (annotations :accessor annotations)
   (message-events :accessor message-events)
   (links :accessor links)))

(defclass annotation (base)
  ((time :accessor timestamp :initarg :timestamp)
   (message :accessor message :initarg :message :type string)
   (attributes :accessor attributes :initarg :attributes)))

(deftype link-type () '(member :unspecified :child :parent))

(defclass link (base)
  ((trace-id :initform nil :accessor trace-id)
   (span-id :initform nil :accessor span-id)
   (link-type :accessor link-type :type link-type)
   (attributes :accessor attributes)))

(deftype message-event-type () '(member :unspecified :sent :recv))
(defclass message-event (base)
  ((timestamp :accessor timestamp)
   (message-id :accessor message-id :initarg :message-id)
   (event-type :accessor event-type)
   (uncompressed-byte-size :accessor uncompressed-byte-size :initarg :uncompressed-byte-size :type integer)
   (compressed-byte-size :accessor compressed-byte-size :initarg :compressed-byte-size :type integer)))

(defmethod name ((s span))
  (span-data-name (span-data s)))

(defmethod span-id ((s span))
  (span-id (span-context s)))

(defmethod parent-span-id ((s span))
  (span-data-parent-span-id (span-data s)))

(defmethod trace-id ((s span))
  (span-context-trace-id (span-context s)))

(defmethod child-span-count ((s span))
  (child-span-count (span-data s)))

(defmethod print-object ((s span) stream)
  (with-slots (data span-context attributes annotations message-events links) s
    (format stream "Span[data=~a, span-context: ~a, attributes: ~a, annotations: ~a, message-events: ~a, links: ~a]"
            data span-context attributes annotations message-events links)))

(defclass span-context (base)
  ((trace-id :accessor trace-id)
   (span-id :accessor span-id)
   (trace-options :accessor trace-options :initform 0)
   (trace-state :accessor trace-state)))

(defmethod sampled-p ((s span-context))
  (not (= 0 (logand 1 (trace-options s)))))

(defmethod (setf sampled-p) (n (s span-context) )
  (setf (trace-options s) (if n
                              (logior 1 (trace-options s))
                              (logand (lognot 1) (trace-options s)))))

(defclass span-data (base)
  ((parent-span-id :accessor parent-span-id)
   (span-context :initarg :span-context :accessor span-context)
   (span-kind :initarg :span-kind :accessor span-kind)
   (name :initarg :name :type string :accessor name)
   (start-time :initform (local-time:now) :accessor start-time)
   (end-time :accessor end-time :accessor end-time)
   (attributes :accessor attributes)
   (annotations :accessor annotations)
   (message-events :accessor message-events)
   (status :accessor status :initform nil)
   (links :accessor links)
   (has-remote-parent :initarg :has-remote-parent :type boolean)
   (dropped-attribute-count :initform 0 :type fixnum :accessor dropped-attribute-count)
   (dropped-annotation-count :initform 0 :type fixnum :accessor dropped-annotation-count)
   (dropped-message-event-count :initform 0 :type fixnum :accessor dropped-message-event-count)
   (dropped-link-count :initform 0 :type fixnum :accessor dropped-link-count)
   (child-span-count :initform 0 :type fixnum :accessor child-span-count)))

(defmethod span-id ((s span-data))
  (span-id (span-context s)))

(defmethod trace-id ((s span-data))
  (trace-id (span-context s)))

(defclass ring-buffer (base)
  ((lock :accessor lock :initform (bt:make-lock))
   (buffer :accessor ring-buffer-buffer :initarg :buffer)
   (head :accessor ring-buffer-head :type fixnum :initform 0)
   (dropped-count :type integer :initform 0)
   (length :initform 0)
   (tail :accessor ring-buffer-tail :initform 0)))

(defmethod dropped-count ((rb ring-buffer))
  (bt:with-lock-held ((lock rb))
    (slot-value rb 'dropped-count)))

(defmethod ring-buffer-length ((rb ring-buffer))
  (bt:with-lock-held ((lock rb))
    (slot-value rb 'length)))

(defmethod print-object ((rb ring-buffer) s)
  (bt:with-lock-held ((lock rb))
    (with-slots (buffer head length tail) rb
     (format s "RingBuffer[head: ~a, tail: ~a, length: ~a, buffer: ~a"
             head tail length buffer))))

(defun make-ring-buffer (size)
  "initializes a ring buffer of size size"
  (make-instance 'ring-buffer :buffer (make-array size)))

(defun ring-buffer-push (rb item)
  "pushes item onto the ringbuffer, threadsafe"
  (bt:with-lock-held ((lock rb))
    (with-slots (buffer head tail length) rb
     (when (> (1+ length) (array-dimension buffer 0))
       (progn
         (incf (slot-value rb 'dropped-count)) ;; avoid mutex
         (setf tail (mod (1+ tail) (array-dimension buffer 0) ))
         (decf length)))
     (setf (elt buffer head) item)
     (setf head (mod (1+ head) (array-dimension buffer 0) ))
     (incf length))))

(defun ring-buffer-pop (rb)
  "pops an item off, returns nil of nothing, threadsafe"
  (bt:with-lock-held ((lock rb))
    (ring-buffer-pop-no-lock rb)))

(defun ring-buffer-pop-no-lock (rb)
  "internal pop method, not thread safe"
  (with-slots (buffer head tail length) rb
    (if (> length 0)
        (let ((item (elt buffer tail)))
          (setf (elt buffer tail) nil)
          (setf tail (mod (1+ tail) (array-dimension buffer 0) ))
          (decf length)
          item)
        nil)))

(defun ring-buffer->list (rb)
  (bt:with-lock-held ((lock rb))
    (loop for x = (ring-buffer-pop-no-lock rb)
         while x
         collect x)))

(defun ring-buffer->array (rb)
  (bt:with-lock-held ((lock rb))
    (let ((a (make-array (ring-buffer-length rb))))
     (loop for x = (ring-buffer-pop-no-lock rb)
           for y from 0 upto (array-dimension a 0)
           while x
           do (setf (aref a y) x)))))
