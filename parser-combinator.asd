(asdf:defsystem parser-combinator
  :version "0"
  :description "Toy implementation of parser combinators"
  :maintainer " <ramarren@cignet.higersbergernet>"
  :author " <ramarren@cignet.higersbergernet>"
  :licence "BSD-style"
  :depends-on (:iterate :alexandria :bpm)
  :components ((:file "package")
	       (:file "basic" :depends-on ("package"))
	       (:file "parsers" :depends-on ("package" "basic"))
	       (:file "memoize" :depends-on ("package"))))
