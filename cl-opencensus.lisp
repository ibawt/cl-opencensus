;;;; cl-opencensus.lisp

(in-package #:cl-opencensus)

(declaim (optimize (debug 3)))
(defun very-random ()
  "returns a 64bit random number from /dev/urandom which is more randomer than random, apparently."
  (with-open-file (file #P"/dev/urandom" :element-type '(unsigned-byte 8))
    (let ((buf (make-array 8 :element-type '(unsigned-byte 8))))
      (read-sequence buf file)
      (logand (intbytes:octets->uint64 buf) most-positive-fixnum))))

(defclass span (base)
  ((data :initform nil :initarg :data :accessor span-data)
   (lock :initform (bt:make-lock) :accessor span-lock)
   (exported :initform nil :accessor span-exported)
   (span-context :accessor span-context :initform nil)
   (attributes :initform nil :accessor span-attributes)
   (annotations :initform nil :accessor span-annotations)
   (message-events :initform nil :accessor message-events)
   (links :initform nil :accessor span-links)))
(export 'span)

(defmethod print-object ((s span) stream)
  (with-slots (data span-context attributes annotations message-events links) s
    (format stream "Span[data=~a, span-context: ~a, attributes: ~a, annotations: ~a, message-events: ~a, links: ~a]"
            data span-context attributes annotations message-events links)))

(defclass span-context (base)
  ((trace-id :accessor span-context-trace-id)
   (span-id :accessor span-context-span-id) trace-options trace-state))

(defclass span-data (base)
  ((parent-span-id :accessor span-data-parent-span-id)
   (span-context :initarg :span-context)
   (span-kind :initarg :span-kind :accessor span-data-span-kind)
   (name :initarg :name :type string :accessor span-data-name)
   (start-time :initform (local-time:now) :accessor start-time)
   (end-time :accessor span-data-end-time :accessor end-time)
   attributes
   annotations message-events status
   links
   (has-remote-parent :initarg :has-remote-parent :type boolean)
   (dropped-attribute-count :initform 0 :type fixnum)
   (dropped-annotation-count :initform 0 :type fixnum)
   (dropped-message-event-count :initform 0 :type fixnum)
   (dropped-link-count :initform 0 :type fixnum)
   (child-span-count :initform 0 :type fixnum :accessor child-span-count)))

(atomics:defstruct atomic-integer
  (value 0 :type #-sbcl fixnum #+sbcl sb-ext:word))

(defclass id-generator (base)
  ((next-span-id :initform (make-atomic-integer) :type atomic-integer)
   (lock :initform (bt:make-lock) :accessor id-generator-lock)
   (span-id-inc :initform 0 :type fixnum :initarg :span-id-inc)
   (trace-id-add :initform (make-array 2 :element-type 'fixnum))
   (random-state :initform (make-random-state t))))

(defmethod print-object ((id id-generator) stream)
  (bt:with-lock-held ((id-generator-lock id))
    (format stream "IdGenerator[next-span-id: ~a, span-id-inc: ~a, trace-id-add: ~a]"
            (atomic-integer-value (slot-value id 'next-span-id))
            (slot-value id 'span-id-inc) (slot-value id 'trace-id-add))))

(defun make-id-generator ()
  (let ((idg (make-instance 'id-generator)))
    (with-slots (next-span-id span-id-inc trace-id-add random-state) idg
      (setf (atomic-integer-value next-span-id) (very-random))
      (setf span-id-inc (logior 1 (very-random)))
      (setf (aref trace-id-add 0) (very-random))
      (setf (aref trace-id-add 1) (very-random)))
    idg))

(defvar *id-generator* (make-id-generator)
  "global id generator")

(defun next-span-id ()
  "returns the next span-id, thread safe via atomics"
  (with-slots (span-id-inc next-span-id) *id-generator*
    (intbytes:int64->octets
     (loop do (atomics:atomic-incf (atomic-integer-value next-span-id) span-id-inc)
           while (= 0 (atomic-integer-value next-span-id))
           finally (return (atomic-integer-value next-span-id))))))

(defun next-trace-id ()
  "returns the a suitable random trace-id, thread safe via mutex"
  (with-slots (lock trace-id-add random-state) *id-generator*
    (bt:with-lock-held (lock)
      (let ((a (+ (random most-positive-fixnum random-state) (aref trace-id-add 0)))
            (b (+ (random most-positive-fixnum random-state) (aref trace-id-add 1))))
        (concatenate '(vector (unsigned-byte 8)) (intbytes:int64->octets a) (intbytes:int64->octets b))))))

(defvar *current-span* nil
  "the current span if any")
(export '*current-span*)

(defmethod add-child ((s span))
  (bt:with-lock-held ((slot-value s 'lock))
    (incf (slot-value (span-data s) 'child-span-count))))

(defun make-span (name &key (span-kind :unspecified) (has-remote-parent nil))
  (let ((parent))
    (when *current-span*
      (add-child *current-span*)
      (setf parent (span-context *current-span*)))
    (let ((s (make-instance 'span)))
      (setf (span-context s) parent)
      (unless parent
        (setf (span-context s) (make-instance 'span-context))
        (setf (span-context-trace-id  (span-context s)) (next-trace-id)))
      (setf (span-context-span-id (span-context s)) (next-span-id))
      (setf (span-data s) (make-instance 'span-data
                                         ;; how do we get span context in here
                                         :span-context parent
                                         :span-kind span-kind
                                         :name name
                                         :has-remote-parent has-remote-parent))

      (when parent
        (setf (span-data-parent-span-id (span-data s)) (span-context-span-id parent)))
      s)))
(export 'make-span)

(defmethod sampled-p ((p span-context))
  t)

(defmethod span->protobuf ((s span))
  (let ((pspan (make-instance 'opencensus.proto.trace.v1:span)))
    (setf (opencensus.proto.trace.v1:trace-id pspan) (span-context-trace-id (span-context s)))
    (setf (opencensus.proto.trace.v1:span-id pspan) (span-context-span-id (span-context s)))
    (setf (opencensus.proto.trace.v1:parent-span-id pspan) (span-data-parent-span-id (span-data s)))
    (setf (opencensus.proto.trace.v1:name pspan) (span-data-name (span-data s)))
    (setf (opencensus.proto.trace.v1:kind pspan) (span-data-span-kind (span-data s)))
    (setf (opencensus.proto.trace.v1:start-time pspan) (start-time (span-data s)))
    (setf (opencensus.proto.trace.v1:end-time pspan) (end-time (span-data s)))
    (setf (opencensus.proto.trace.v1:child-span-count pspan) (child-span-count (span-data s)))
    pspan))

(defun finish-span ()
  (unless *current-span*
    (error "no spans to finish!"))
  (bt:with-lock-held ((span-lock *current-span*))
    (unless (span-exported *current-span*)
      (if (sampled-p (span-context *current-span*))
          (setf (span-data-end-time (span-data *current-span*)) (local-time:now))
          (export-span *current-span*)) ;; serialize in the output thread to avoid tracing overhead
      (setf (span-exported *current-span*) t))))
(export 'finish-span)

(defmacro with-span (name &body body)
  `(let ((*current-span* (make-span ,name)))
     (unwind-protect
          (progn
            ,@body)
       (finish-span))))
(export 'with-span)
