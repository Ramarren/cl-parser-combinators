(asdf:defsystem parser-combinators-cl-ppcre
  :version "0"
  :description "An implementation of parser combinators for Common Lisp"
  :maintainer "Jakub Higersberger <ramarren@gmail.com>"
  :author "Jakub Higersberger <ramarren@gmail.com>"
  :licence "BSD-style"
  :depends-on (:iterate :alexandria :cl-ppcre :parser-combinators)
  :components ((:file "regex-parser")))