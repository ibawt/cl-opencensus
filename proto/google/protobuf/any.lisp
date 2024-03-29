
;;;;    google/protobuf/any.lisp

;;; Generated by the protocol buffer compiler.  DO NOT EDIT!


(cl:in-package #:common-lisp-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:google.protobuf)
    (make-package '#:google.protobuf :use nil)))
(in-package #:google.protobuf)
(cl:declaim #.com.google.base:*optimize-default*)

(cl:defclass any (pb:protocol-buffer)
  (
  (type-url
   :accessor type-url
   :initform (pb:string-field "")
   :type pb::%sf%)
  (value
   :accessor value
   :initform (cl:make-array 0 :element-type '(cl:unsigned-byte 8))
   :type (cl:simple-array (cl:unsigned-byte 8) (cl:*)))
  (%has-bits%
   :accessor %has-bits%
   :initform 0
   :type (cl:unsigned-byte 2))
  (pb::%cached-size%
   :initform 0
   :type (cl:integer 0 #.(cl:1- cl:array-dimension-limit)))
  ))

(cl:export 'any)

(cl:export 'type-url)


(cl:defmethod (cl:setf type-url) :after (x (self any))
  (cl:declare (cl:ignore x))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))

(cl:unless (cl:fboundp 'has-type-url)
  (cl:defgeneric has-type-url (proto)))
(cl:defmethod has-type-url ((self any))
  (cl:logbitp 0 (cl:slot-value self '%has-bits%)))
(cl:export 'has-type-url)

(cl:unless (cl:fboundp 'clear-type-url)
  (cl:defgeneric clear-type-url (proto)))
(cl:defmethod clear-type-url ((self any))
  (cl:setf (cl:slot-value self 'type-url) (pb:string-field ""))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 0)
  (cl:values))
(cl:export 'clear-type-url)

(cl:export 'value)


(cl:defmethod (cl:setf value) :after (x (self any))
  (cl:declare (cl:ignore x))
  (cl:setf (cl:ldb (cl:byte 1 1) (cl:slot-value self '%has-bits%)) 1))

(cl:unless (cl:fboundp 'has-value)
  (cl:defgeneric has-value (proto)))
(cl:defmethod has-value ((self any))
  (cl:logbitp 1 (cl:slot-value self '%has-bits%)))
(cl:export 'has-value)

(cl:unless (cl:fboundp 'clear-value)
  (cl:defgeneric clear-value (proto)))
(cl:defmethod clear-value ((self any))
  (cl:setf (cl:slot-value self 'value) (cl:make-array 0 :element-type '(cl:unsigned-byte 8)))
  (cl:setf (cl:ldb (cl:byte 1 1) (cl:slot-value self '%has-bits%)) 0)
  (cl:values))
(cl:export 'clear-value)


(cl:defmethod cl:print-object ((self any) stream)
  (cl:pprint-logical-block (stream cl:nil)
    (cl:print-unreadable-object (self stream :type cl:t :identity cl:t)
      (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
        (cl:format stream " ~_type-url: ~s" (type-url self)))
      (cl:when (cl:logbitp 1 (cl:slot-value self '%has-bits%))
        (cl:format stream " ~_value: ~s" (value self)))
      ))
  (cl:values))

(cl:defmethod pb:clear ((self any))
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf (cl:slot-value self 'type-url) (pb:string-field "")))
  (cl:when (cl:logbitp 1 (cl:slot-value self '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:make-array 0 :element-type '(cl:unsigned-byte 8))))
  (cl:setf (cl:slot-value self '%has-bits%) 0)
  (cl:values))

(cl:defmethod pb:is-initialized ((self any))
  cl:t)

(cl:defmethod pb:octet-size ((self any))
  (cl:let ((size 0))
    ;; string type_url = 1[json_name = "typeUrl"];
    (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
      (cl:incf size 1)
      (cl:incf size (cl:let ((s (pb::%utf8-string-length% (cl:slot-value self 'type-url))))
        (cl:+ s (varint:length32 s)))))
    ;; bytes value = 2[json_name = "value"];
    (cl:when (cl:logbitp 1 (cl:slot-value self '%has-bits%))
      (cl:incf size 1)
      (cl:incf size (cl:let ((s (cl:length (cl:slot-value self 'value))))
        (cl:+ s (varint:length32 s)))))
    (cl:setf (cl:slot-value self 'pb::%cached-size%) size)
    size))

(cl:defmethod pb:serialize ((self any) buffer index limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index index limit)
              (cl:ignorable buffer limit))
  ;; string type_url = 1[json_name = "typeUrl"];
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit 10))
    (cl:setf index (wire-format:write-octets-carefully buffer index limit (cl:slot-value (cl:slot-value self 'type-url) 'pb::%octets%))))
  ;; bytes value = 2[json_name = "value"];
  (cl:when (cl:logbitp 1 (cl:slot-value self '%has-bits%))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit 18))
    (cl:setf index (wire-format:write-octets-carefully buffer index limit (cl:slot-value self 'value))))
  index)

(cl:defmethod pb:merge-from-array ((self any) buffer start limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index start limit))
  (cl:do ((index start index))
      ((cl:>= index limit) index)
    (cl:declare (cl:type com.google.base:vector-index index))
    (cl:multiple-value-bind (tag new-index)
        (varint:parse-uint32-carefully buffer index limit)
      (cl:setf index new-index)
      (cl:case tag
        ;; string type_url = 1[json_name = "typeUrl"];
        ((10)
          (cl:multiple-value-bind (value new-index)
              (wire-format:read-octets-carefully buffer index limit)
            (cl:setf (cl:slot-value self 'type-url) (pb:string-field value))
            (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1)
            (cl:setf index new-index)))
        ;; bytes value = 2[json_name = "value"];
        ((18)
          (cl:multiple-value-bind (value new-index)
              (wire-format:read-octets-carefully buffer index limit)
            (cl:setf (cl:slot-value self 'value) value)
            (cl:setf (cl:ldb (cl:byte 1 1) (cl:slot-value self '%has-bits%)) 1)
            (cl:setf index new-index)))
        (cl:t
          (cl:when (cl:= (cl:logand tag 7) 4)
            (cl:return-from pb:merge-from-array index))
          (cl:setf index (wire-format:skip-field buffer index limit tag)))))))

(cl:defmethod pb:merge-from-message ((self any) (from any))
  (cl:when (cl:logbitp 0 (cl:slot-value from '%has-bits%))
    (cl:setf (cl:slot-value self 'type-url) (cl:slot-value from 'type-url))
    (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))
  (cl:when (cl:logbitp 1 (cl:slot-value from '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:slot-value from 'value))
    (cl:setf (cl:ldb (cl:byte 1 1) (cl:slot-value self '%has-bits%)) 1))
)


