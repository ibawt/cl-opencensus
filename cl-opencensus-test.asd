(asdf:defsystem #:cl-opencensus-test
  :description "Describe cl-opencensus here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-opencensus
               #:clack
               #:fiveam)
  :components ((:file "test/package")
               (:file "test/cl-opencensus")))
