;;;; cl-opencensus.asd

(asdf:defsystem #:cl-opencensus
  :description "Describe cl-opencensus here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:com.google.base
               #:cl-intbytes
               #:atomics
               #:local-time
               #:drakma
               #:protobuf
               #:bordeaux-threads)
  :in-order-to ((test-op (test-op "cl-opencensus-test")))
  :components ((:file "package")
               (:file "proto/google/protobuf/type")
               (:file "proto/google/protobuf/any")
               (:file "proto/google/protobuf/wrappers")
               (:file "proto/google/protobuf/timestamp")
               (:file "proto/common")
               (:file "proto/resource")
               (:file "proto/metrics")
               (:file "proto/metrics_service")
               (:file "proto/stats")
               (:file "proto/trace_config")
               (:file "proto/trace")
               (:file "proto/trace_service")
               (:file "base")
               (:file "sampler")
               (:file "lrumap")
               (:file "log")
               (:file "exporter")
               (:file "cl-opencensus")))
