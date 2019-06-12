(in-package #:cl-opencensus)

(defconstant +buffer-size+ 512)

(defvar *exporter-url* "http://127.0.0.1:55678")
(defvar *current-exporter* nil)
(export '*current-exporter*)

(defclass exporter (base)
  ((lock :initform (bt:make-lock) :reader exporter-lock)
   (running :initform t :type boolean :accessor exporter-running)
   (wakeup :initform (bt:make-condition-variable) :reader wakeup)
   (thread :initarg :thread :accessor exporter-thread)
   (input-buffer :initform (make-ring-buffer +buffer-size+) :accessor input-buffer :documentation "contains span-data")
   (output-buffer :initform (make-ring-buffer +buffer-size+) :accessor output-buffer :documentation "contains pb spans")))
(export 'exporter)

(defclass debug-exporter (exporter)
  ((spans :initform '() :accessor spans)))
(export 'debug-exporter)

(defmacro with-exporter-lock (e &body body)
  `(bt:with-lock-held ((exporter-lock ,e))
     ,@body))

(defmacro with-debug-exporter (&body body)
  `(let* ((*current-exporter* (make-instance 'debug-exporter)))
    ,@body))
(export 'with-debug-exporter)

(defmacro with-exporter (&body body)
  `(let* ((current-exporter (make-instance 'exporter))
          (*current-exporter* current-exporter))
     (setf (exporter-thread current-exporter)
           (bt:make-thread
            (lambda ()
              (let ((*current-exporter* current-exporter))
                (run-exporter)))
             :name "exporter-thread"))
     (unwind-protect (progn
                       ,@body)
       (stop-exporter current-exporter))))
(export 'with-exporter)

(defun stop-exporter (current-exporter)
  (bt:with-lock-held ((exporter-lock current-exporter))
    (setf (exporter-running current-exporter) nil)
    (bt:condition-notify (wakeup current-exporter)))
  (bt:join-thread (exporter-thread current-exporter)))
(export 'stop-exporter)

(defun exporter-running-p ()
  (bt:with-lock-held ((exporter-lock *current-exporter*))
    (exporter-running *current-exporter*)))

(defconstant +flush-interval+ (* internal-time-units-per-second 60))

(defun fill-output-buffer ()
  (loop for x = (ring-buffer-pop (input-buffer *current-exporter*))
        while x
        do (ring-buffer-push (output-buffer *current-exporter*) (span->protobuf x))))

(defconstant +max-octets-per-bundle+ (* 1024 1024))

(defun fetch-bundle ()
  (let* ((len 0)
         (bundle (loop with octet-sum = 0
                       for s = (ring-buffer-pop (input-buffer *current-exporter*))
                       while (and s (< octet-sum +max-octets-per-bundle+))
                       do (setf octet-sum (+ (pb:octet-size s) octet-sum))
                       do (incf len)
                       collect s)))
    (make-array len :element-type 'opencensus.proto.trace.v1:span
                :initial-contents bundle)))

(defun make-trace-service-request (spans)
  (let ((export-request (make-instance 'opencensus.proto.agent.trace.v1:export-trace-service-request)))
    (setf (opencensus.proto.agent.trace.v1:spans export-request) spans)
    export-request))

(defun serialize-protobuf (pb)
  (let* ((size (pb:octet-size pb))
         (buf (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize pb buf 0 size)
    buf))

(defconstant +max-fails-per-flush+ 5)

(defun flush-buffer ()
  (let ((bundle (fetch-bundle)))
    (when bundle
      (let ((req-buffer (serialize-protobuf (make-trace-service-request bundle)))
            (fails 0))
        (loop :until (> fails +max-fails-per-flush+)
              :do (handler-case
                      (multiple-value-bind (body status-code)
                          (drakma:http-request *exporter-url*
                                               :method :post
                                               :content-type "application/protobuf"
                                               :content req-buffer)
                        (case (floor (/ status-code 100))
                          (2 (let ((r (make-instance 'opencensus.proto.agent.trace.v1:export-trace-service-response)))
                               (pb:merge-from-array r body 0 (length body))
                               (return r)))
                          (otherwise
                           (incf fails))))
                    (drakma:drakma-error (e)
                      (incf fails)
                      (log/error "caught error in flush: ~a" e)))
                  (sleep (+ (random 0.5) (* 0.20 fails fails))))))))

(defun buffer-needs-flushing-p ()
  (cond
    ((> (ring-buffer-length (output-buffer *current-exporter*)) 0) t)
    ((> (ring-buffer-length (input-buffer *current-exporter*)) 0) t)
    (t nil)))

(defmethod export-span ((e debug-exporter) (span span-data))
  (with-exporter-lock e
    (push span (spans e))))

(defmethod export-span ((e exporter)(span span-data))
  (ring-buffer-push (input-buffer e) span))

(defun run-exporter ()
  (let ((*random-state* (make-random-state t)))
    (loop :while (exporter-running-p)
          :do (let ((t1 (get-internal-real-time)))
                (fill-output-buffer)
                (flush-buffer)
                (let* ((t2 (- (get-internal-real-time) t1) )
                       (sleep (float (/ (max 0 (- +flush-interval+ t2)) internal-time-units-per-second))))
                  (if (and (not (buffer-needs-flushing-p)) (> sleep 0))
                      (with-exporter-lock *current-exporter*
                          (bt:condition-wait (wakeup *current-exporter*) (exporter-lock *current-exporter*) :timeout sleep))
                      (bt:thread-yield)))))))
