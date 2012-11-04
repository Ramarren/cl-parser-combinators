(asdf:defsystem parser-combinators-tests
  :version "0"
  :description "Test for parser combinator library"
  :maintainer "Jakub Higersberger <ramarren@gmail.com>"
  :author "Jakub Higersberger <ramarren@gmail.com>"
  :licence "BSD-style"
  :depends-on (:hu.dwim.stefil :iterate :alexandria :parser-combinators :infix)
  :components ((:file "test-root")
               (:file "test-basic" :depends-on ("test-root"))
               (:file "test-parsers" :depends-on ("test-root"))
               (:file "test-arithmetic" :depends-on ("test-root"))
               (:file "test-expression" :depends-on ("test-root" "test-arithmetic"))))
