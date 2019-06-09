(in-package #:cl-opencensus)

(defclass base ()
  ())

(defclass config (base)
  ((lock :accessor lock :initform (bt:make-lock))
   (default-sampler :accessor default-sampler)
   (max-annotation-events-per-span :accessor max-annotations-events-per-span :initform 32)
   (max-message-events-per-span :accessor max-message-events-per-span :initform 128)
   (max-attributes-per-span :accessor max-attributes-per-span :initform 32)
   (max-links-per-span :accessor max-links-per-span :initform 32))
  (:documentation "configuration for cl-opencensus"))
(defvar *config* (make-instance 'config))

(defclass span (base)
  ((data :initform nil :initarg :data :accessor span-data)
   (lock :initform (bt:make-lock) :accessor span-lock)
   (exported :initform nil :accessor span-exported)
   (span-context :accessor span-context :initform nil)
   (attributes :initform nil :accessor attributes)
   (annotations :initform nil :accessor annotations)
   (message-events :initform nil :accessor message-events)
   (links :initform nil :accessor links)))

(deftype link-type () '(member :unspecified :child :parent))

(defclass link (base)
  ((trace-id :initform nil :accessor trace-id)
   (span-id :initform nil :accessor span-id)
   (link-type :accessor link-type :type link-type)
   (attributes :accessor attributes)))

(deftype message-event-type () '(member :unspecified :sent :recv))
(defclass message-event (base)
  ((timestamp :accessor timestamp)
   (event-type :accessor event-type)
   (uncompressed-byte-size :accessor uncompressed-byte-size)
   (compressed-byte-size :accessor compressed-byte-size)))

(defmethod name ((s span))
  (span-data-name (span-data s)))

(defmethod span-id ((s span))
  (span-context-span-id (span-context s)))

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
  ((trace-id :accessor span-context-trace-id)
   (span-id :accessor span-context-span-id)
   (trace-options :accessor trace-options :initform 0)
   (trace-state :accessor trace-state)))

(defmethod sampled-p ((s span-context))
  (not (= 0 (logand 1 (trace-options s)))))

(defmethod (setf sampled-p) ((s span-context) n)
  (setf (trace-options s) (logior 1 (trace-options s))))

(defclass span-data (base)
  ((parent-span-id :accessor span-data-parent-span-id)
   (span-context :initarg :span-context)
   (span-kind :initarg :span-kind :accessor span-data-span-kind)
   (name :initarg :name :type string :accessor span-data-name)
   (start-time :initform (local-time:now) :accessor start-time)
   (end-time :accessor span-data-end-time :accessor end-time)
   (attributes :accessor attributes)
   (annotations :accessor annotations)
   (message-events :accessor message-events)
   status
   (links :accessor links)
   (has-remote-parent :initarg :has-remote-parent :type boolean)
   (dropped-attribute-count :initform 0 :type fixnum)
   (dropped-annotation-count :initform 0 :type fixnum)
   (dropped-message-event-count :initform 0 :type fixnum)
   (dropped-link-count :initform 0 :type fixnum)
   (child-span-count :initform 0 :type fixnum :accessor child-span-count)))

(defclass ring-buffer (base)
  ((buffer :accessor ring-buffer-buffer :initarg :buffer)
   (head :accessor ring-buffer-head :type fixnum :initform 0)
   (dropped-count :accessor dropped-count :type integer :initform 0)
   (length :accessor ring-buffer-length :initform 0)
   (tail :accessor ring-buffer-tail :initform 0)))

(defmethod print-object ((rb ring-buffer) s)
  (with-slots (buffer head length tail) rb
    (format s "RingBuffer[head: ~a, tail: ~a, length: ~a, buffer: ~a"
            head tail length buffer)))

(defun make-ring-buffer (size)
  (make-instance 'ring-buffer
                 :buffer (make-array size)))

(defun ring-buffer-push (rb item)
  (with-slots (buffer head tail length) rb
    (when (> (1+ length) (array-dimension buffer 0))
      (progn
        (incf (dropped-count rb))
        (setf tail (mod (1+ tail) (array-dimension buffer 0) ))
        (decf length)))
    (setf (elt buffer head) item)
    (setf head (mod (1+ head) (array-dimension buffer 0) ))
    (incf length)))

(defun ring-buffer-pop (rb)
  (with-slots (buffer head tail length) rb
    (if (> length 0)
        (let ((item (elt buffer tail)))
          (setf (elt buffer tail) nil)
          (setf tail (mod (1+ tail) (array-dimension buffer 0) ))
          (decf length)
          item)
        nil)))

(defun ring-buffer->list (rb)
  (loop for x = (ring-buffer-pop rb)
        while x
        collect x))
