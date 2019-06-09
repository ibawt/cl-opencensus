(in-package #:cl-opencensus)

(defconstant +buffer-size+ 512)
(defconstant +flush-interval+ (* internal-time-units-per-second 60))
(defconstant +max-fails-per-flush+ 5)

(defvar *exporter-url* "http://127.0.0.1:55678")
(defvar *current-exporter* nil)
(export '*current-exporter*)

(defclass exporter (base)
  ((lock :initform (bt:make-lock) :reader exporter-lock)
   (running :initform t :type boolean :accessor exporter-running)
   (wakeup :initform (bt:make-condition-variable) :reader wakeup)
   (thread :initarg :thread :accessor exporter-thread)
   (buffer :initform (make-ring-buffer +buffer-size+) :accessor exporter-buffer)))
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

(defun fetch-bundle ()
  (bt:with-lock-held ((exporter-lock *current-exporter*))
    (loop for s = (ring-buffer-pop (exporter-buffer *current-exporter*))
          while s
          collect s)))

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


(defun flush-buffer ()
  (let ((bundles (fetch-bundle))
        (fails 0))
    (loop :while bundles
          :until (> fails +max-fails-per-flush+)
          :finally (when bundles
                     (bt:with-lock-held ((exporter-lock *current-exporter*))
                      (dolist (b bundles) (ring-buffer-push (exporter-buffer *current-exporter*) b))))
          :do (let* ((bundle (pop bundles))
                     (buf (when bundle (serialize-protobuf bundle))))
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
                             (setf fails 0)))
                        (otherwise
                         (progn
                           (push bundle bundles)
                           (incf fails)))))
                  (drakma:drakma-error (e)
                    (incf fails)
                    (push bundle bundles)
                    (log/error "caught error in flush: ~a" e)))
                (when (> fails 0)
                  (sleep (+ (random 0.5) (* 0.20 fails fails))))))))

(defmethod export-span ((e debug-exporter) (span span))
  (with-exporter-lock e
    (push span (spans e))))

(defmethod export-span ((e exporter)(span span))
  (with-exporter-lock e
    (ring-buffer-push (exporter-buffer e) span)))

(defun run-exporter ()
  (let ((*random-state* (make-random-state t)))
    (loop :while (exporter-running-p)
          :do (let ((t1 (get-internal-real-time)))
                (flush-buffer)
                (let* ((t2 (- (get-internal-real-time) t1) )
                       (sleep (float (/ (max 0 (- +flush-interval+ t2)) internal-time-units-per-second))))
                  (if (> sleep 0)
                      (with-exporter-lock *current-exporter*
                          (bt:condition-wait (wakeup *current-exporter*) (exporter-lock *current-exporter*) :timeout sleep))
                      (bt:thread-yield)))))))
