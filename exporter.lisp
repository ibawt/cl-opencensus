(in-package #:cl-opencensus)

(defconstant +buffer-size+ 512)
(declaim (optimize (debug 3)))
(defvar *exporter-url* "http://127.0.0.1:55678")
(defvar *current-exporter* nil)
(export '*current-exporter*)

(defvar *log-lock* (bt:make-lock))

(defclass ring-buffer (base)
  ((buffer :accessor ring-buffer-buffer :initarg :buffer)
   (head :accessor ring-buffer-head :type 'fixnum :initform 0)
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

(defclass exporter (base)
  ((lock :initform (bt:make-lock) :reader exporter-lock)
   (running :initform t :type boolean :accessor exporter-running)
   (thread :initarg :thread :accessor exporter-thread)
   (buffer :initform (make-ring-buffer +buffer-size+) :accessor exporter-buffer)))
(export 'exporter)

(defmacro with-exporter (&body body)
  `(let* ((current-exporter (make-instance 'exporter))
          (*current-exporter* current-exporter))
     (setf (exporter-thread current-exporter)
           (bt:make-thread
            (lambda ()
              (let ((*current-exporter* current-exporter))
                (logz :info "about to run exporter")
                (run-exporter)))
             :name "exporter-thread"))
     (logz :info "after making the thread")
     (unwind-protect (progn
                       ,@body
                       (logz :debug "exiting exporter body"))
       (stop-exporter current-exporter))))

(export 'with-exporter)

(defun stop-exporter (current-exporter)
  (logz :info "stop-exporter")
  (bt:with-lock-held ((exporter-lock current-exporter))
    (setf (exporter-running current-exporter) nil))
  (bt:join-thread (exporter-thread current-exporter))
  (logz :info "exporter thread stopped!"))
(export 'stop-exporter)

(defun exporter-running-p ()
  (bt:with-lock-held ((exporter-lock *current-exporter*))
    (exporter-running *current-exporter*)))

(defconstant +flush-interval+ (* internal-time-units-per-second (* 30 60)))

(defun fetch-bundle ()
  (bt:with-lock-held ((exporter-lock *current-exporter*))
    (loop for s = (ring-buffer-pop (exporter-buffer *current-exporter*))
          while s
          collect s)))

(defun logz (level fmt &rest rest)
  (bt:with-lock-held (*log-lock*)
    (format t "[~a]: " level)
    (apply #'format t fmt rest)
    (format t "~%")
    (finish-output)))
(export 'logz)

(defun spans->protobuf (spans)
  (make-array (length spans)
              :element-type 'opencensus.proto.trace.v1:span
              :initial-contents (mapcar #'span->protobuf spans)))

(defun make-trace-service-request (spans)
  (let ((export-request (make-instance 'opencensus.proto.agent.trace.v1:export-trace-service-request)))
    (setf (opencensus.proto.agent.trace.v1:spans export-request)
          (spans->protobuf spans))
    export-request))

(defun serialize-protobuf (pb)
  (let* ((size (pb:octet-size pb))
        (buf (make-array size :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (pb:serialize pb buf 0 size)
    buf))

(defconstant +max-fails-per-flush+ 5)

(defun flush-buffer ()
  (let ((bundles (fetch-bundle))
        (fails 0))
    (loop :while bundles
          :until (> fails +max-fails-per-flush+)
          :finally (when bundles
                     (bt:with-lock-held ((exporter-lock *current-exporter*))
                      (dolist (b bundles) (ring-buffer-push (exporter-buffer *current-exporter*) b))))
          :do (let* ((bundle (pop bundles))
                     (buf (serialize-protobuf bundle)))
                (logz :debug "bundle: ~a" bundle)
                (unless bundle (return))
                (handler-case
                    (multiple-value-bind (body status-code)
                        (drakma:http-request *exporter-url*
                                             :method :post
                                             :content-type "application/protobuf"
                                             :content buf)
                      (case (floor (/ status-code 100))
                        (2 (let ((r (make-instance 'opencensus.proto.agent.trace.v1:export-trace-service-response)))
                             (pb:merge-from-array r body 0 (length body))
                             (setf fails 0)
                             (logz :info "response-body: ~a" r)))
                        (otherwise
                         (progn
                           (push bundle bundles)
                           (incf fails)
                           (logz :error "invalid response code: ~a" status-code)))))
                  (drakma:drakma-error (e)
                    (incf fails)
                    (push bundle bundles)
                    (logz :error "caught error in flush: ~a" e)))
                (when (> fails 0)
                  (sleep (+ (random 0.5) (* 0.20 fails fails))))))))

(defun export-span (span)
  (logz :info "export-span")
  (break)
  (bt:with-lock-held ((exporter-lock *current-exporter*))
    (ring-buffer-push (exporter-buffer *current-exporter*) span)))

(defun run-exporter ()
  (let ((*random-state* (make-random-state t)))
   (loop :while (exporter-running-p)
         :do (let ((t1 (get-internal-real-time)))
               (flush-buffer)
               (let* ((time (- (get-internal-real-time) t1))
                      (sleep-time (float (/ internal-time-units-per-second (- +flush-interval+ time)))))
                 (if (>= sleep-time 0)
                     (sleep 0.5))
                 (bt:thread-yield))))))
