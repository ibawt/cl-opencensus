(ql:quickload 'cl-opencensus-test)
(in-package #:cl-opencensus)

(unless (fiveam:results-status (fiveam:run 'opencensus-tests))
  (uiop:quit 1))
