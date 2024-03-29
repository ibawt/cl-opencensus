;;;; cl-opencensus.lisp

(in-package #:cl-opencensus)
(declaim (optimize (debug 3)))

(defun very-random ()
  "returns a 64bit random number from /dev/urandom which is more randomer than random, apparently."
  (with-open-file (file #P"/dev/urandom" :element-type '(unsigned-byte 8))
    (let ((buf (make-array 8 :element-type '(unsigned-byte 8))))
      (read-sequence buf file)
      (logand (intbytes:octets->uint64 buf) most-positive-fixnum))))

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
  (bt:with-lock-held ((span-lock s))
    (incf (child-span-count (span-data s)))))

(defmethod span->protobuf ((s span-data))
  (let ((pspan (make-instance 'opencensus.proto.trace.v1:span)))
    (setf (opencensus.proto.trace.v1:trace-id pspan) (trace-id (span-context s)))
    (setf (opencensus.proto.trace.v1:span-id pspan) (span-id (span-context s)))
    (setf (opencensus.proto.trace.v1:parent-span-id pspan) (parent-span-id (span-data s)))
    (setf (opencensus.proto.trace.v1:name pspan) (name (span-data s)))
    (setf (opencensus.proto.trace.v1:kind pspan) (span-kind (span-data s)))
    (setf (opencensus.proto.trace.v1:start-time pspan) (start-time (span-data s)))
    (setf (opencensus.proto.trace.v1:end-time pspan) (end-time (span-data s)))
    (setf (opencensus.proto.trace.v1:child-span-count pspan) (child-span-count (span-data s)))
    pspan))

(defun make-span-data (s)
  (let ((sd (span-data s)))
    (setf (dropped-attribute-count sd) (dropped-count (attributes s)))
    (setf (attributes sd) (attribute-map->list (attributes s)))

    (setf (dropped-annotation-count sd) (dropped-count (annotations s)))
    (setf (annotations sd) (ring-buffer->list (annotations s)))

    (setf (dropped-message-event-count sd) (dropped-count (annotations s)))
    (setf (message-events sd) (ring-buffer->list (message-events s)))

    (setf (dropped-link-count sd) (dropped-count (links s)))
    (setf (links sd) (ring-buffer->list (links s)))

    sd))

(defun finish-span ()
  (unless *current-span*
    (error "no spans to finish!"))
  (bt:with-lock-held ((span-lock *current-span*))
    (if (span-exported *current-span*)
        (error "span already exported")
        (progn
          (when (sampled-p (span-context *current-span*))
            (setf (end-time (span-data *current-span*)) (local-time:now))
            (export-span *current-exporter* (make-span-data *current-span*)))
          (setf (span-exported *current-span*) t)
          (setf (span-data *current-span*) nil)))))
(export 'finish-span)

(defmethod recording-events-p ((s span))
  (not (not (span-data s))))

(defun make-span (name &key (remote-parent nil) (span-kind :unspecified) (parent-span-context nil) (sampler nil))
  "creates a span instance"
  (let ((parent parent-span-context))
    (when *current-span*
      (add-child *current-span*)
      (setf parent (span-context *current-span*)))
    (let ((s (make-instance 'span)))
      (setf (span-context s) parent)
      (unless parent
        (setf (span-context s) (make-instance 'span-context))
        (setf (trace-id  (span-context s)) (next-trace-id)))
      (setf (span-id (span-context s)) (next-span-id))

      (when (or (not parent-span-context) remote-parent sampler)
        (setf (sampled-p (span-context s)) (sample (or sampler (default-sampler *config*))
                                                   (make-instance 'sampling-parameters
                                                                  :parent-context parent
                                                                  :trace-id (trace-id (span-context s))
                                                                  :span-id (span-id (span-context s))
                                                                  :name name
                                                                  :has-remote-parent remote-parent))))
      (when (sampled-p (span-context s))
        (setf (span-data s) (make-instance 'span-data
                                           :span-context (span-context s)
                                           :span-kind span-kind
                                           :name name
                                           :has-remote-parent remote-parent))
        (setf (attributes s) (make-instance 'attribute-map :max-count (max-attributes-per-span *config*)))
        (setf (annotations s) (make-ring-buffer (max-annotations-events-per-span *config*)))
        (setf (message-events s) (make-ring-buffer (max-message-events-per-span *config*)))
        (setf (links s) (make-ring-buffer (max-links-per-span *config*))))
      (when parent
        (setf (parent-span-id (span-data s)) (span-id parent)))
      s)))
(export 'make-span)

(defmacro with-span (name (&optional (parent-context nil)
                             (kind :unspecified) (sampler nil)
                             (remote-parent nil)) &body body)
  `(let ((*current-span* (make-span ,name :parent-span-context ,parent-context
                                          :remote-parent ,remote-parent
                                          :span-kind ,kind
                                          :sampler ,sampler)))
     (unwind-protect
          (progn
            ,@body)
       (finish-span))))
(export 'with-span)

(defun add-attributes (&rest attrs)
  (when (recording-events-p *current-span*)
    (loop for a in attrs
          do (attribute-map-add (attributes *current-span*) (car a) (cadr a)))))
(export 'add-attributes)

(defun add-annotation (attributes fmt &rest rest)
  (when (recording-events-p *current-span*)
    (let ((t1 (local-time:now))
          (s (apply #'format nil fmt rest)))
      (let ((a (make-instance 'annotation :timestamp t1
                                          :message s :attributes attributes)))
        (ring-buffer-push (annotations *current-span*) a)))))
(export 'add-annotation)

(defun add-message-event (message-id uncompressed-byte-size compressed-byte-size type)
  (when (recording-events-p *current-span*)
    (let ((t1 (local-time:now)))
      (ring-buffer-push (message-events *current-span*)
                        (make-instance 'message-event
                                       :message-id message-id
                                       :timestamp t1
                                       :event-type type
                                       :uncompressed-byte-size uncompressed-byte-size
                                       :compressed-byte-size compressed-byte-size)))))
(export 'add-message-event)

