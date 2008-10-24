(asdf:defsystem parser-combinator-tests
  :version "0"
  :description "Test for parser combinator library"
  :maintainer "Jakub Higersberger <ramarren@gmail.com>"
  :author "Jakub Higersberger <ramarren@gmail.com>"
  :licence "BSD-style"
  :depends-on (:stefil :iterate :alexandria :parser-combinator :infix)
  :components ((:file "test-root")
	       (:file "test-parsers" :depends-on ("test-root"))
	       (:file "test-arithmetic" :depends-on ("test-root"))))

