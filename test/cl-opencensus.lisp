(in-package #:cl-opencensus-test)

(def-suite opencensus-tests :description "all tests")
(in-suite opencensus-tests)
(declaim (optimize (debug 3)))
(defvar *handler* nil)

(defmacro with-http-server (handler &body body)
  `(let ((server (clack:clackup ,handler)))
     (format t "server running~%")
     (finish-output)
     (unwind-protect (progn ,@body)
       (clack:stop server))))

(test exporter
  (format nil "gonna run a test")
  (with-http-server (lambda (env)
                      (declare (ignore env))
                      '(200 (:content-type "text/plain") ("Hello World")))
    (with-exporter
      (with-span "foo"
        (is (= 3 (+ 1 2)))))))

