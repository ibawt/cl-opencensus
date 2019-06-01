(in-package #:cl-opencensus)

(defconstant +buffer-size+ 512)

(defvar *exporter-url* "http://127.0.0.1:55678")

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
   (thread :initarg :thread :reader exporter-thread)
   (buffer :initform (make-ring-buffer +buffer-size+) :accessor exporter-buffer)))

(defvar *current-exporter* nil)

(defmacro with-exporter (&body body)
  `(let* ((*current-exporter* (make-instance 'exporter)))
     (setf (slot-value 'thread (bt:make-thread (lambda ()
                                                 (run-exporter)))))

     (unwind-protect (progn ,@body)
       (stop-exporter))))


(defun stop-exporter ()
  (bt:with-lock-held ((exporter-lock *current-exporter*))
    (setf (exporter-running *current-exporter*) nil))
  (bt:join-thread (exporter-thread *current-exporter*)))

(defun exporter-running-p ()
  (bt:with-lock-held ((exporter-lock *current-exporter*))
    (exporter-running *current-exporter*)))

(defconstant +flush-interval+ (* internal-time-units-per-second (* 30 60)))

(defun fetch-bundle ()
  (bt:with-lock-held ((exporter-lock *current-exporter*))
    (loop for s = (ring-buffer-pop (exporter-buffer *current-exporter*))
          while s
          collect s)))

(defun connect ()
  (drakma:http-request *exporter-url* :method :post
                                      :content-type "application/protobuf"
                                      :want-stream t))

(defun logz (level fmt &rest rest)
  (format t "[~a]" level)
  (apply #'format t fmt rest)
  (format t "~%")
  (finish-output))

(defun serialize-span (s)
  (let* ((pbuf (span->protobuf s))
         (size (pb:octet-size pbuf))
         (buf (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize pbuf buf 0 size)
    buf))

(defun flush-buffer ()
  (let ((bundles (fetch-bundle))
        (fails 0)
        (stream (connect)))
    (unwind-protect
         (loop while bundles
               do (let ((b (pop bundles)))
                    (handler-case
                        (progn
                          (write-sequence (serialize-span b) stream)
                          (setf fails 0))
                      (drakma:drakma-error (e)
                        (logz :error "caught error in flush: ~a" e)
                        (push bundles b)
                        (sleep (+ (random 0.5) (* 0.20 fails fails)))
                        (incf fails)
                        (setf stream (connect))))))
      (close stream))))

(defun export-span (span)
  (bt:with-lock-held ((exporter-lock *current-exporter*))
    (ring-buffer-push (exporter-buffer *current-exporter*) span)))

(defun run-exporter ()
  (loop :while (exporter-running-p)
        :do (let ((t1 (get-internal-real-time)))
              (flush-buffer)
              (let* ((time (- (get-internal-real-time) t1))
                     (sleep-time (- +flush-interval+ time)))
                (sleep (/ sleep-time internal-time-units-per-second))))))
