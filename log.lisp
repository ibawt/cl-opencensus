(in-package #:cl-opencensus)

(defconstant +error+ 0)
(defconstant +warn+  1)
(defconstant +info+  2)
(defconstant +debug+ 3)

(deftype log-level () '(member +error+ +warn+ +info+ +debug+))

(defclass logger (base)
  ((lock :initform (bt:make-lock) :accessor logger-lock)
   (level :initform :info :initarg :level :accessor logger-level)
   (stream :initarg :stream :initform *standard-output* :accessor logger-stream)))
(export 'logger)

(defvar *current-logger* (make-instance 'logger))
(export '*current-logger*)

(defmacro with-logger (logger &body body)
  `(let ((*current-logger* ,logger))
     ,@body))
(export 'with-logger)

(defun log/info (fmt &rest rest)
  (logz +info+ fmt rest))
(export 'log/info)

(defun log/warn (fmt &rest rest)
  (logz +warn+ fmt rest))
(export 'log/warn)

(defun log/error (fmt &rest rest)
  (logz +error+ fmt rest))
(export 'log/error)

(defun log/debug (fmt &rest rest)
  (logz +debug+ fmt rest))
(export 'log/debug)

(defun logz (level fmt &rest rest)
  (when (>= level (logger-level *current-logger*))
    (with-accessors ((stream logger-stream))
        *current-logger*
      (bt:with-lock-held ((logger-lock *current-logger*))
        (format stream "[~a]: " level)
        (apply #'format stream fmt rest)
        (format stream "~%")
        (finish-output)))))
