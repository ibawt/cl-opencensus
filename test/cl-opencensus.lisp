(in-package #:cl-opencensus)

(fiveam:def-suite opencensus-tests :description "all tests")
(fiveam:in-suite opencensus-tests)

(defmacro with-http-server (handler &body body)
  `(let ((server (clack:clackup ,handler :port 55678)))
     (unwind-protect (progn ,@body)
       (clack:stop server))))

(fiveam:test ring-buffer-test
  (let ((rb (make-ring-buffer 3)))
    (ring-buffer-push rb 0)
    (ring-buffer-push rb 1)
    (ring-buffer-push rb 2)
    (ring-buffer-push rb 3)
    (fiveam:is (equalp '(1 2 3) (ring-buffer->list rb)))
    (fiveam:is (= 1 (dropped-count rb)))))

(fiveam:test attribute-map-test
  (let ((a (make-instance 'attribute-map :max-count 1)))
    (attribute-map-add a :bar :foo)
    (attribute-map-add a :foo :bar)
    (fiveam:is (equalp '((:foo :bar)) (attribute-map->list a))))
  (let ((a (make-instance 'attribute-map :max-count 3)))
    (attribute-map-add a :a 1)
    (attribute-map-add a :b 2)
    (attribute-map-add a :a 4)
    (fiveam:is (equalp '((:a 4) (:b 2)) (attribute-map->list a)))
    (attribute-map-add a :z 54)
    (fiveam:is (equalp '((:z 54) (:a 4) (:b 2)) (attribute-map->list a)))))

(fiveam:test simple-span-test
  (with-debug-exporter
    (with-span "foo" ()
      (+ 1 2))
    (let ((spans (spans *current-exporter*)))
      (fiveam:is (= 1 (length spans)))
      (fiveam:is (span-id (car spans)))
      (fiveam:is (trace-id (car spans)))
      (fiveam:is (equal "foo" (name (car spans)))))))

(fiveam:test nested-span-test
  (with-debug-exporter
    (with-span "foo" ()
      (with-span "bar" () ))
    (let* ((spans (spans *current-exporter*))
           (root (car spans))
           (child (cadr spans)))
      (fiveam:is (= 2 (length spans)))
      (fiveam:is (equal "foo" (name root)))
      (fiveam:is (equal "bar" (name child)))
      (fiveam:is (not (equal nil (span-id root))))
      (fiveam:is (not (equal nil (parent-span-id child))))
      (fiveam:is (equalp (span-id (car spans)) (parent-span-id (cadr spans))))
      (fiveam:is (= 1 (child-span-count root)))
      (fiveam:is (equalp (trace-id root) (trace-id child))))))
