(ql:quickload 'cl-opencensus-test)
(in-package #:cl-opencensus)

(defun main ()
  (unless (fiveam:results-status (fiveam:run 'opencensus-tests))
    (uiop:quit 1)))
