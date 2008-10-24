(asdf:defsystem parser-combinator
  :version "0"
  :description "Toy implementation of parser combinators"
  :maintainer "Jakub Higersberger <ramarren@gmail.com>"
  :author "Jakub Higersberger <ramarren@gmail.com>"
  :licence "BSD-style"
  :depends-on (:iterate :alexandria :bpm)
  :components ((:file "package")
	       (:file "lazy" :depends-on ("package"))
	       (:file "basic" :depends-on ("package" "lazy"))
	       (:file "combinators" :depends-on ("package" "basic" "lazy"))
	       (:file "primitives" :depends-on ("package" "lazy" "basic" "combinators"))
	       (:file "parsers" :depends-on ("package" "basic" "primitives" "combinators"))
	       (:file "memoize" :depends-on ("package"))
	       (:file "greedy" :depends-on ("package" "basic" "primitives" "combinators"))))
