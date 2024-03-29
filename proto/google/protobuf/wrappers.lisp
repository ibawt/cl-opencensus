
;;;;    google/protobuf/wrappers.lisp

;;; Generated by the protocol buffer compiler.  DO NOT EDIT!


(cl:in-package #:common-lisp-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:google.protobuf)
    (make-package '#:google.protobuf :use nil)))
(in-package #:google.protobuf)
(cl:declaim #.com.google.base:*optimize-default*)

(cl:defclass double-value (pb:protocol-buffer)
  (
  (value
   :accessor value
   :initform 0d0
   :type cl:double-float)
  (%has-bits%
   :accessor %has-bits%
   :initform 0
   :type (cl:unsigned-byte 1))
  (pb::%cached-size%
   :initform 0
   :type (cl:integer 0 #.(cl:1- cl:array-dimension-limit)))
  ))

(cl:export 'double-value)

(cl:export 'value)


(cl:defmethod (cl:setf value) :after (x (self double-value))
  (cl:declare (cl:ignore x))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))

(cl:unless (cl:fboundp 'has-value)
  (cl:defgeneric has-value (proto)))
(cl:defmethod has-value ((self double-value))
  (cl:logbitp 0 (cl:slot-value self '%has-bits%)))
(cl:export 'has-value)

(cl:unless (cl:fboundp 'clear-value)
  (cl:defgeneric clear-value (proto)))
(cl:defmethod clear-value ((self double-value))
  (cl:setf (cl:slot-value self 'value) 0d0)
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 0)
  (cl:values))
(cl:export 'clear-value)


(cl:defmethod cl:print-object ((self double-value) stream)
  (cl:pprint-logical-block (stream cl:nil)
    (cl:print-unreadable-object (self stream :type cl:t :identity cl:t)
      (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
        (cl:format stream " ~_value: ~s" (value self)))
      ))
  (cl:values))

(cl:defmethod pb:clear ((self double-value))
  (cl:setf (cl:slot-value self 'value) 0d0)
  (cl:setf (cl:slot-value self '%has-bits%) 0)
  (cl:values))

(cl:defmethod pb:is-initialized ((self double-value))
  cl:t)

(cl:defmethod pb:octet-size ((self double-value))
  (cl:let ((size 0))
    ;; double value = 1[json_name = "value"];
    (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
      (cl:incf size
        (cl:+ 1 8)))
    (cl:setf (cl:slot-value self 'pb::%cached-size%) size)
    size))

(cl:defmethod pb:serialize ((self double-value) buffer index limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index index limit)
              (cl:ignorable buffer limit))
  ;; double value = 1[json_name = "value"];
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit 9))
    (cl:setf index (wire-format:write-double-float-carefully buffer index limit (cl:slot-value self 'value))))
  index)

(cl:defmethod pb:merge-from-array ((self double-value) buffer start limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index start limit))
  (cl:do ((index start index))
      ((cl:>= index limit) index)
    (cl:declare (cl:type com.google.base:vector-index index))
    (cl:multiple-value-bind (tag new-index)
        (varint:parse-uint32-carefully buffer index limit)
      (cl:setf index new-index)
      (cl:case tag
        ;; double value = 1[json_name = "value"];
        ((9)
          (cl:multiple-value-bind (value new-index)
              (wire-format:read-double-float-carefully buffer index limit)
            (cl:setf (cl:slot-value self 'value) value)
            (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1)
            (cl:setf index new-index)))
        (cl:t
          (cl:when (cl:= (cl:logand tag 7) 4)
            (cl:return-from pb:merge-from-array index))
          (cl:setf index (wire-format:skip-field buffer index limit tag)))))))

(cl:defmethod pb:merge-from-message ((self double-value) (from double-value))
  (cl:when (cl:logbitp 0 (cl:slot-value from '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:slot-value from 'value))
    (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))
)


(cl:defclass float-value (pb:protocol-buffer)
  (
  (value
   :accessor value
   :initform 0f0
   :type cl:single-float)
  (%has-bits%
   :accessor %has-bits%
   :initform 0
   :type (cl:unsigned-byte 1))
  (pb::%cached-size%
   :initform 0
   :type (cl:integer 0 #.(cl:1- cl:array-dimension-limit)))
  ))

(cl:export 'float-value)

(cl:export 'value)


(cl:defmethod (cl:setf value) :after (x (self float-value))
  (cl:declare (cl:ignore x))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))

(cl:unless (cl:fboundp 'has-value)
  (cl:defgeneric has-value (proto)))
(cl:defmethod has-value ((self float-value))
  (cl:logbitp 0 (cl:slot-value self '%has-bits%)))
(cl:export 'has-value)

(cl:unless (cl:fboundp 'clear-value)
  (cl:defgeneric clear-value (proto)))
(cl:defmethod clear-value ((self float-value))
  (cl:setf (cl:slot-value self 'value) 0f0)
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 0)
  (cl:values))
(cl:export 'clear-value)


(cl:defmethod cl:print-object ((self float-value) stream)
  (cl:pprint-logical-block (stream cl:nil)
    (cl:print-unreadable-object (self stream :type cl:t :identity cl:t)
      (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
        (cl:format stream " ~_value: ~s" (value self)))
      ))
  (cl:values))

(cl:defmethod pb:clear ((self float-value))
  (cl:setf (cl:slot-value self 'value) 0f0)
  (cl:setf (cl:slot-value self '%has-bits%) 0)
  (cl:values))

(cl:defmethod pb:is-initialized ((self float-value))
  cl:t)

(cl:defmethod pb:octet-size ((self float-value))
  (cl:let ((size 0))
    ;; float value = 1[json_name = "value"];
    (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
      (cl:incf size
        (cl:+ 1 4)))
    (cl:setf (cl:slot-value self 'pb::%cached-size%) size)
    size))

(cl:defmethod pb:serialize ((self float-value) buffer index limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index index limit)
              (cl:ignorable buffer limit))
  ;; float value = 1[json_name = "value"];
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit 13))
    (cl:setf index (wire-format:write-single-float-carefully buffer index limit (cl:slot-value self 'value))))
  index)

(cl:defmethod pb:merge-from-array ((self float-value) buffer start limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index start limit))
  (cl:do ((index start index))
      ((cl:>= index limit) index)
    (cl:declare (cl:type com.google.base:vector-index index))
    (cl:multiple-value-bind (tag new-index)
        (varint:parse-uint32-carefully buffer index limit)
      (cl:setf index new-index)
      (cl:case tag
        ;; float value = 1[json_name = "value"];
        ((13)
          (cl:multiple-value-bind (value new-index)
              (wire-format:read-single-float-carefully buffer index limit)
            (cl:setf (cl:slot-value self 'value) value)
            (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1)
            (cl:setf index new-index)))
        (cl:t
          (cl:when (cl:= (cl:logand tag 7) 4)
            (cl:return-from pb:merge-from-array index))
          (cl:setf index (wire-format:skip-field buffer index limit tag)))))))

(cl:defmethod pb:merge-from-message ((self float-value) (from float-value))
  (cl:when (cl:logbitp 0 (cl:slot-value from '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:slot-value from 'value))
    (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))
)


(cl:defclass int64value (pb:protocol-buffer)
  (
  (value
   :accessor value
   :initform 0
   :type (cl:signed-byte 64))
  (%has-bits%
   :accessor %has-bits%
   :initform 0
   :type (cl:unsigned-byte 1))
  (pb::%cached-size%
   :initform 0
   :type (cl:integer 0 #.(cl:1- cl:array-dimension-limit)))
  ))

(cl:export 'int64value)

(cl:export 'value)


(cl:defmethod (cl:setf value) :after (x (self int64value))
  (cl:declare (cl:ignore x))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))

(cl:unless (cl:fboundp 'has-value)
  (cl:defgeneric has-value (proto)))
(cl:defmethod has-value ((self int64value))
  (cl:logbitp 0 (cl:slot-value self '%has-bits%)))
(cl:export 'has-value)

(cl:unless (cl:fboundp 'clear-value)
  (cl:defgeneric clear-value (proto)))
(cl:defmethod clear-value ((self int64value))
  (cl:setf (cl:slot-value self 'value) 0)
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 0)
  (cl:values))
(cl:export 'clear-value)


(cl:defmethod cl:print-object ((self int64value) stream)
  (cl:pprint-logical-block (stream cl:nil)
    (cl:print-unreadable-object (self stream :type cl:t :identity cl:t)
      (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
        (cl:format stream " ~_value: ~s" (value self)))
      ))
  (cl:values))

(cl:defmethod pb:clear ((self int64value))
  (cl:setf (cl:slot-value self 'value) 0)
  (cl:setf (cl:slot-value self '%has-bits%) 0)
  (cl:values))

(cl:defmethod pb:is-initialized ((self int64value))
  cl:t)

(cl:defmethod pb:octet-size ((self int64value))
  (cl:let ((size 0))
    ;; int64 value = 1[json_name = "value"];
    (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
      (cl:incf size
        (cl:+ 1 (varint:length64 (cl:ldb (cl:byte 64 0) (cl:slot-value self 'value))))))
    (cl:setf (cl:slot-value self 'pb::%cached-size%) size)
    size))

(cl:defmethod pb:serialize ((self int64value) buffer index limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index index limit)
              (cl:ignorable buffer limit))
  ;; int64 value = 1[json_name = "value"];
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit 8))
    (cl:setf index (varint:encode-uint64-carefully buffer index limit (cl:ldb (cl:byte 64 0) (cl:slot-value self 'value)))))
  index)

(cl:defmethod pb:merge-from-array ((self int64value) buffer start limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index start limit))
  (cl:do ((index start index))
      ((cl:>= index limit) index)
    (cl:declare (cl:type com.google.base:vector-index index))
    (cl:multiple-value-bind (tag new-index)
        (varint:parse-uint32-carefully buffer index limit)
      (cl:setf index new-index)
      (cl:case tag
        ;; int64 value = 1[json_name = "value"];
        ((8)
          (cl:multiple-value-bind (value new-index)
              (varint:parse-int64-carefully buffer index limit)
            (cl:setf (cl:slot-value self 'value) value)
            (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1)
            (cl:setf index new-index)))
        (cl:t
          (cl:when (cl:= (cl:logand tag 7) 4)
            (cl:return-from pb:merge-from-array index))
          (cl:setf index (wire-format:skip-field buffer index limit tag)))))))

(cl:defmethod pb:merge-from-message ((self int64value) (from int64value))
  (cl:when (cl:logbitp 0 (cl:slot-value from '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:slot-value from 'value))
    (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))
)


(cl:defclass uint64value (pb:protocol-buffer)
  (
  (value
   :accessor value
   :initform 0
   :type (cl:unsigned-byte 64))
  (%has-bits%
   :accessor %has-bits%
   :initform 0
   :type (cl:unsigned-byte 1))
  (pb::%cached-size%
   :initform 0
   :type (cl:integer 0 #.(cl:1- cl:array-dimension-limit)))
  ))

(cl:export 'uint64value)

(cl:export 'value)


(cl:defmethod (cl:setf value) :after (x (self uint64value))
  (cl:declare (cl:ignore x))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))

(cl:unless (cl:fboundp 'has-value)
  (cl:defgeneric has-value (proto)))
(cl:defmethod has-value ((self uint64value))
  (cl:logbitp 0 (cl:slot-value self '%has-bits%)))
(cl:export 'has-value)

(cl:unless (cl:fboundp 'clear-value)
  (cl:defgeneric clear-value (proto)))
(cl:defmethod clear-value ((self uint64value))
  (cl:setf (cl:slot-value self 'value) 0)
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 0)
  (cl:values))
(cl:export 'clear-value)


(cl:defmethod cl:print-object ((self uint64value) stream)
  (cl:pprint-logical-block (stream cl:nil)
    (cl:print-unreadable-object (self stream :type cl:t :identity cl:t)
      (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
        (cl:format stream " ~_value: ~s" (value self)))
      ))
  (cl:values))

(cl:defmethod pb:clear ((self uint64value))
  (cl:setf (cl:slot-value self 'value) 0)
  (cl:setf (cl:slot-value self '%has-bits%) 0)
  (cl:values))

(cl:defmethod pb:is-initialized ((self uint64value))
  cl:t)

(cl:defmethod pb:octet-size ((self uint64value))
  (cl:let ((size 0))
    ;; uint64 value = 1[json_name = "value"];
    (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
      (cl:incf size
        (cl:+ 1 (varint:length64 (cl:slot-value self 'value)))))
    (cl:setf (cl:slot-value self 'pb::%cached-size%) size)
    size))

(cl:defmethod pb:serialize ((self uint64value) buffer index limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index index limit)
              (cl:ignorable buffer limit))
  ;; uint64 value = 1[json_name = "value"];
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit 8))
    (cl:setf index (varint:encode-uint64-carefully buffer index limit (cl:slot-value self 'value))))
  index)

(cl:defmethod pb:merge-from-array ((self uint64value) buffer start limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index start limit))
  (cl:do ((index start index))
      ((cl:>= index limit) index)
    (cl:declare (cl:type com.google.base:vector-index index))
    (cl:multiple-value-bind (tag new-index)
        (varint:parse-uint32-carefully buffer index limit)
      (cl:setf index new-index)
      (cl:case tag
        ;; uint64 value = 1[json_name = "value"];
        ((8)
          (cl:multiple-value-bind (value new-index)
              (varint:parse-uint64-carefully buffer index limit)
            (cl:setf (cl:slot-value self 'value) value)
            (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1)
            (cl:setf index new-index)))
        (cl:t
          (cl:when (cl:= (cl:logand tag 7) 4)
            (cl:return-from pb:merge-from-array index))
          (cl:setf index (wire-format:skip-field buffer index limit tag)))))))

(cl:defmethod pb:merge-from-message ((self uint64value) (from uint64value))
  (cl:when (cl:logbitp 0 (cl:slot-value from '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:slot-value from 'value))
    (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))
)


(cl:defclass int32value (pb:protocol-buffer)
  (
  (value
   :accessor value
   :initform 0
   :type (cl:signed-byte 32))
  (%has-bits%
   :accessor %has-bits%
   :initform 0
   :type (cl:unsigned-byte 1))
  (pb::%cached-size%
   :initform 0
   :type (cl:integer 0 #.(cl:1- cl:array-dimension-limit)))
  ))

(cl:export 'int32value)

(cl:export 'value)


(cl:defmethod (cl:setf value) :after (x (self int32value))
  (cl:declare (cl:ignore x))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))

(cl:unless (cl:fboundp 'has-value)
  (cl:defgeneric has-value (proto)))
(cl:defmethod has-value ((self int32value))
  (cl:logbitp 0 (cl:slot-value self '%has-bits%)))
(cl:export 'has-value)

(cl:unless (cl:fboundp 'clear-value)
  (cl:defgeneric clear-value (proto)))
(cl:defmethod clear-value ((self int32value))
  (cl:setf (cl:slot-value self 'value) 0)
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 0)
  (cl:values))
(cl:export 'clear-value)


(cl:defmethod cl:print-object ((self int32value) stream)
  (cl:pprint-logical-block (stream cl:nil)
    (cl:print-unreadable-object (self stream :type cl:t :identity cl:t)
      (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
        (cl:format stream " ~_value: ~s" (value self)))
      ))
  (cl:values))

(cl:defmethod pb:clear ((self int32value))
  (cl:setf (cl:slot-value self 'value) 0)
  (cl:setf (cl:slot-value self '%has-bits%) 0)
  (cl:values))

(cl:defmethod pb:is-initialized ((self int32value))
  cl:t)

(cl:defmethod pb:octet-size ((self int32value))
  (cl:let ((size 0))
    ;; int32 value = 1[json_name = "value"];
    (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
      (cl:incf size
        (cl:+ 1 (varint:length64 (cl:ldb (cl:byte 64 0) (cl:slot-value self 'value))))))
    (cl:setf (cl:slot-value self 'pb::%cached-size%) size)
    size))

(cl:defmethod pb:serialize ((self int32value) buffer index limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index index limit)
              (cl:ignorable buffer limit))
  ;; int32 value = 1[json_name = "value"];
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit 8))
    (cl:setf index (varint:encode-uint64-carefully buffer index limit (cl:ldb (cl:byte 64 0) (cl:slot-value self 'value)))))
  index)

(cl:defmethod pb:merge-from-array ((self int32value) buffer start limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index start limit))
  (cl:do ((index start index))
      ((cl:>= index limit) index)
    (cl:declare (cl:type com.google.base:vector-index index))
    (cl:multiple-value-bind (tag new-index)
        (varint:parse-uint32-carefully buffer index limit)
      (cl:setf index new-index)
      (cl:case tag
        ;; int32 value = 1[json_name = "value"];
        ((8)
          (cl:multiple-value-bind (value new-index)
              (varint:parse-int32-carefully buffer index limit)
            (cl:setf (cl:slot-value self 'value) value)
            (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1)
            (cl:setf index new-index)))
        (cl:t
          (cl:when (cl:= (cl:logand tag 7) 4)
            (cl:return-from pb:merge-from-array index))
          (cl:setf index (wire-format:skip-field buffer index limit tag)))))))

(cl:defmethod pb:merge-from-message ((self int32value) (from int32value))
  (cl:when (cl:logbitp 0 (cl:slot-value from '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:slot-value from 'value))
    (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))
)


(cl:defclass uint32value (pb:protocol-buffer)
  (
  (value
   :accessor value
   :initform 0
   :type (cl:unsigned-byte 32))
  (%has-bits%
   :accessor %has-bits%
   :initform 0
   :type (cl:unsigned-byte 1))
  (pb::%cached-size%
   :initform 0
   :type (cl:integer 0 #.(cl:1- cl:array-dimension-limit)))
  ))

(cl:export 'uint32value)

(cl:export 'value)


(cl:defmethod (cl:setf value) :after (x (self uint32value))
  (cl:declare (cl:ignore x))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))

(cl:unless (cl:fboundp 'has-value)
  (cl:defgeneric has-value (proto)))
(cl:defmethod has-value ((self uint32value))
  (cl:logbitp 0 (cl:slot-value self '%has-bits%)))
(cl:export 'has-value)

(cl:unless (cl:fboundp 'clear-value)
  (cl:defgeneric clear-value (proto)))
(cl:defmethod clear-value ((self uint32value))
  (cl:setf (cl:slot-value self 'value) 0)
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 0)
  (cl:values))
(cl:export 'clear-value)


(cl:defmethod cl:print-object ((self uint32value) stream)
  (cl:pprint-logical-block (stream cl:nil)
    (cl:print-unreadable-object (self stream :type cl:t :identity cl:t)
      (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
        (cl:format stream " ~_value: ~s" (value self)))
      ))
  (cl:values))

(cl:defmethod pb:clear ((self uint32value))
  (cl:setf (cl:slot-value self 'value) 0)
  (cl:setf (cl:slot-value self '%has-bits%) 0)
  (cl:values))

(cl:defmethod pb:is-initialized ((self uint32value))
  cl:t)

(cl:defmethod pb:octet-size ((self uint32value))
  (cl:let ((size 0))
    ;; uint32 value = 1[json_name = "value"];
    (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
      (cl:incf size
        (cl:+ 1 (varint:length32 (cl:slot-value self 'value)))))
    (cl:setf (cl:slot-value self 'pb::%cached-size%) size)
    size))

(cl:defmethod pb:serialize ((self uint32value) buffer index limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index index limit)
              (cl:ignorable buffer limit))
  ;; uint32 value = 1[json_name = "value"];
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit 8))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit (cl:slot-value self 'value))))
  index)

(cl:defmethod pb:merge-from-array ((self uint32value) buffer start limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index start limit))
  (cl:do ((index start index))
      ((cl:>= index limit) index)
    (cl:declare (cl:type com.google.base:vector-index index))
    (cl:multiple-value-bind (tag new-index)
        (varint:parse-uint32-carefully buffer index limit)
      (cl:setf index new-index)
      (cl:case tag
        ;; uint32 value = 1[json_name = "value"];
        ((8)
          (cl:multiple-value-bind (value new-index)
              (varint:parse-uint32-carefully buffer index limit)
            (cl:setf (cl:slot-value self 'value) value)
            (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1)
            (cl:setf index new-index)))
        (cl:t
          (cl:when (cl:= (cl:logand tag 7) 4)
            (cl:return-from pb:merge-from-array index))
          (cl:setf index (wire-format:skip-field buffer index limit tag)))))))

(cl:defmethod pb:merge-from-message ((self uint32value) (from uint32value))
  (cl:when (cl:logbitp 0 (cl:slot-value from '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:slot-value from 'value))
    (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))
)


(cl:defclass bool-value (pb:protocol-buffer)
  (
  (value
   :accessor value
   :initform cl:nil
   :type cl:boolean)
  (%has-bits%
   :accessor %has-bits%
   :initform 0
   :type (cl:unsigned-byte 1))
  (pb::%cached-size%
   :initform 0
   :type (cl:integer 0 #.(cl:1- cl:array-dimension-limit)))
  ))

(cl:export 'bool-value)

(cl:export 'value)


(cl:defmethod (cl:setf value) :after (x (self bool-value))
  (cl:declare (cl:ignore x))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))

(cl:unless (cl:fboundp 'has-value)
  (cl:defgeneric has-value (proto)))
(cl:defmethod has-value ((self bool-value))
  (cl:logbitp 0 (cl:slot-value self '%has-bits%)))
(cl:export 'has-value)

(cl:unless (cl:fboundp 'clear-value)
  (cl:defgeneric clear-value (proto)))
(cl:defmethod clear-value ((self bool-value))
  (cl:setf (cl:slot-value self 'value) cl:nil)
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 0)
  (cl:values))
(cl:export 'clear-value)


(cl:defmethod cl:print-object ((self bool-value) stream)
  (cl:pprint-logical-block (stream cl:nil)
    (cl:print-unreadable-object (self stream :type cl:t :identity cl:t)
      (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
        (cl:format stream " ~_value: ~s" (value self)))
      ))
  (cl:values))

(cl:defmethod pb:clear ((self bool-value))
  (cl:setf (cl:slot-value self 'value) cl:nil)
  (cl:setf (cl:slot-value self '%has-bits%) 0)
  (cl:values))

(cl:defmethod pb:is-initialized ((self bool-value))
  cl:t)

(cl:defmethod pb:octet-size ((self bool-value))
  (cl:let ((size 0))
    ;; bool value = 1[json_name = "value"];
    (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
      (cl:incf size
        (cl:+ 1 1)))
    (cl:setf (cl:slot-value self 'pb::%cached-size%) size)
    size))

(cl:defmethod pb:serialize ((self bool-value) buffer index limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index index limit)
              (cl:ignorable buffer limit))
  ;; bool value = 1[json_name = "value"];
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit 8))
    (cl:setf index (wire-format:write-boolean-carefully buffer index limit (cl:slot-value self 'value))))
  index)

(cl:defmethod pb:merge-from-array ((self bool-value) buffer start limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index start limit))
  (cl:do ((index start index))
      ((cl:>= index limit) index)
    (cl:declare (cl:type com.google.base:vector-index index))
    (cl:multiple-value-bind (tag new-index)
        (varint:parse-uint32-carefully buffer index limit)
      (cl:setf index new-index)
      (cl:case tag
        ;; bool value = 1[json_name = "value"];
        ((8)
          (cl:multiple-value-bind (value new-index)
              (wire-format:read-boolean-carefully buffer index limit)
            (cl:setf (cl:slot-value self 'value) value)
            (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1)
            (cl:setf index new-index)))
        (cl:t
          (cl:when (cl:= (cl:logand tag 7) 4)
            (cl:return-from pb:merge-from-array index))
          (cl:setf index (wire-format:skip-field buffer index limit tag)))))))

(cl:defmethod pb:merge-from-message ((self bool-value) (from bool-value))
  (cl:when (cl:logbitp 0 (cl:slot-value from '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:slot-value from 'value))
    (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))
)


(cl:defclass string-value (pb:protocol-buffer)
  (
  (value
   :accessor value
   :initform (pb:string-field "")
   :type pb::%sf%)
  (%has-bits%
   :accessor %has-bits%
   :initform 0
   :type (cl:unsigned-byte 1))
  (pb::%cached-size%
   :initform 0
   :type (cl:integer 0 #.(cl:1- cl:array-dimension-limit)))
  ))

(cl:export 'string-value)

(cl:export 'value)


(cl:defmethod (cl:setf value) :after (x (self string-value))
  (cl:declare (cl:ignore x))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))

(cl:unless (cl:fboundp 'has-value)
  (cl:defgeneric has-value (proto)))
(cl:defmethod has-value ((self string-value))
  (cl:logbitp 0 (cl:slot-value self '%has-bits%)))
(cl:export 'has-value)

(cl:unless (cl:fboundp 'clear-value)
  (cl:defgeneric clear-value (proto)))
(cl:defmethod clear-value ((self string-value))
  (cl:setf (cl:slot-value self 'value) (pb:string-field ""))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 0)
  (cl:values))
(cl:export 'clear-value)


(cl:defmethod cl:print-object ((self string-value) stream)
  (cl:pprint-logical-block (stream cl:nil)
    (cl:print-unreadable-object (self stream :type cl:t :identity cl:t)
      (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
        (cl:format stream " ~_value: ~s" (value self)))
      ))
  (cl:values))

(cl:defmethod pb:clear ((self string-value))
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (pb:string-field "")))
  (cl:setf (cl:slot-value self '%has-bits%) 0)
  (cl:values))

(cl:defmethod pb:is-initialized ((self string-value))
  cl:t)

(cl:defmethod pb:octet-size ((self string-value))
  (cl:let ((size 0))
    ;; string value = 1[json_name = "value"];
    (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
      (cl:incf size 1)
      (cl:incf size (cl:let ((s (pb::%utf8-string-length% (cl:slot-value self 'value))))
        (cl:+ s (varint:length32 s)))))
    (cl:setf (cl:slot-value self 'pb::%cached-size%) size)
    size))

(cl:defmethod pb:serialize ((self string-value) buffer index limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index index limit)
              (cl:ignorable buffer limit))
  ;; string value = 1[json_name = "value"];
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit 10))
    (cl:setf index (wire-format:write-octets-carefully buffer index limit (cl:slot-value (cl:slot-value self 'value) 'pb::%octets%))))
  index)

(cl:defmethod pb:merge-from-array ((self string-value) buffer start limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index start limit))
  (cl:do ((index start index))
      ((cl:>= index limit) index)
    (cl:declare (cl:type com.google.base:vector-index index))
    (cl:multiple-value-bind (tag new-index)
        (varint:parse-uint32-carefully buffer index limit)
      (cl:setf index new-index)
      (cl:case tag
        ;; string value = 1[json_name = "value"];
        ((10)
          (cl:multiple-value-bind (value new-index)
              (wire-format:read-octets-carefully buffer index limit)
            (cl:setf (cl:slot-value self 'value) (pb:string-field value))
            (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1)
            (cl:setf index new-index)))
        (cl:t
          (cl:when (cl:= (cl:logand tag 7) 4)
            (cl:return-from pb:merge-from-array index))
          (cl:setf index (wire-format:skip-field buffer index limit tag)))))))

(cl:defmethod pb:merge-from-message ((self string-value) (from string-value))
  (cl:when (cl:logbitp 0 (cl:slot-value from '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:slot-value from 'value))
    (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))
)


(cl:defclass bytes-value (pb:protocol-buffer)
  (
  (value
   :accessor value
   :initform (cl:make-array 0 :element-type '(cl:unsigned-byte 8))
   :type (cl:simple-array (cl:unsigned-byte 8) (cl:*)))
  (%has-bits%
   :accessor %has-bits%
   :initform 0
   :type (cl:unsigned-byte 1))
  (pb::%cached-size%
   :initform 0
   :type (cl:integer 0 #.(cl:1- cl:array-dimension-limit)))
  ))

(cl:export 'bytes-value)

(cl:export 'value)


(cl:defmethod (cl:setf value) :after (x (self bytes-value))
  (cl:declare (cl:ignore x))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))

(cl:unless (cl:fboundp 'has-value)
  (cl:defgeneric has-value (proto)))
(cl:defmethod has-value ((self bytes-value))
  (cl:logbitp 0 (cl:slot-value self '%has-bits%)))
(cl:export 'has-value)

(cl:unless (cl:fboundp 'clear-value)
  (cl:defgeneric clear-value (proto)))
(cl:defmethod clear-value ((self bytes-value))
  (cl:setf (cl:slot-value self 'value) (cl:make-array 0 :element-type '(cl:unsigned-byte 8)))
  (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 0)
  (cl:values))
(cl:export 'clear-value)


(cl:defmethod cl:print-object ((self bytes-value) stream)
  (cl:pprint-logical-block (stream cl:nil)
    (cl:print-unreadable-object (self stream :type cl:t :identity cl:t)
      (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
        (cl:format stream " ~_value: ~s" (value self)))
      ))
  (cl:values))

(cl:defmethod pb:clear ((self bytes-value))
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:make-array 0 :element-type '(cl:unsigned-byte 8))))
  (cl:setf (cl:slot-value self '%has-bits%) 0)
  (cl:values))

(cl:defmethod pb:is-initialized ((self bytes-value))
  cl:t)

(cl:defmethod pb:octet-size ((self bytes-value))
  (cl:let ((size 0))
    ;; bytes value = 1[json_name = "value"];
    (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
      (cl:incf size 1)
      (cl:incf size (cl:let ((s (cl:length (cl:slot-value self 'value))))
        (cl:+ s (varint:length32 s)))))
    (cl:setf (cl:slot-value self 'pb::%cached-size%) size)
    size))

(cl:defmethod pb:serialize ((self bytes-value) buffer index limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index index limit)
              (cl:ignorable buffer limit))
  ;; bytes value = 1[json_name = "value"];
  (cl:when (cl:logbitp 0 (cl:slot-value self '%has-bits%))
    (cl:setf index (varint:encode-uint32-carefully buffer index limit 10))
    (cl:setf index (wire-format:write-octets-carefully buffer index limit (cl:slot-value self 'value))))
  index)

(cl:defmethod pb:merge-from-array ((self bytes-value) buffer start limit)
  (cl:declare (cl:type com.google.base:octet-vector buffer)
              (cl:type com.google.base:vector-index start limit))
  (cl:do ((index start index))
      ((cl:>= index limit) index)
    (cl:declare (cl:type com.google.base:vector-index index))
    (cl:multiple-value-bind (tag new-index)
        (varint:parse-uint32-carefully buffer index limit)
      (cl:setf index new-index)
      (cl:case tag
        ;; bytes value = 1[json_name = "value"];
        ((10)
          (cl:multiple-value-bind (value new-index)
              (wire-format:read-octets-carefully buffer index limit)
            (cl:setf (cl:slot-value self 'value) value)
            (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1)
            (cl:setf index new-index)))
        (cl:t
          (cl:when (cl:= (cl:logand tag 7) 4)
            (cl:return-from pb:merge-from-array index))
          (cl:setf index (wire-format:skip-field buffer index limit tag)))))))

(cl:defmethod pb:merge-from-message ((self bytes-value) (from bytes-value))
  (cl:when (cl:logbitp 0 (cl:slot-value from '%has-bits%))
    (cl:setf (cl:slot-value self 'value) (cl:slot-value from 'value))
    (cl:setf (cl:ldb (cl:byte 1 0) (cl:slot-value self '%has-bits%)) 1))
)


