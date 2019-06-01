
(asdf:defsystem #:cl-opencensus-test
  :description "Describe cl-opencensus here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-opencensus
               #:fiveam)
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam '#:run!
                                      (uiop:find-symbol* '#:cl-opencensus-test-suite
                                                         :cl-opencensus-test)))
  :components ((:file "test/package")
               (:file "test/cl-opencensus")))
