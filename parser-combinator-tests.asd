(asdf:defsystem parser-combinator-tests
  :version "0"
  :description "Test for parser combinator library"
  :maintainer " <ramarren@cignet.higersbergernet>"
  :author " <ramarren@cignet.higersbergernet>"
  :licence "BSD-style"
  :depends-on (:stefil :iterate :alexandria :parser-combinator)
  :components ((:file "test-root")))

