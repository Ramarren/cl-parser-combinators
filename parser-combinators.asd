(asdf:defsystem parser-combinators
  :version "0"
  :description "An implementation of parser combinators for Common Lisp"
  :maintainer "Jakub Higersberger <ramarren@gmail.com>"
  :author "Jakub Higersberger <ramarren@gmail.com>"
  :licence "BSD-style"
  :depends-on (:iterate :alexandria)
  :components ((:file "package")
               (:file "queue" :depends-on ("package"))
               (:file "contexts" :depends-on ("package"))
               (:file "lazy" :depends-on ("package"))
               (:file "cache" :depends-on ("package"))
               (:file "basic" :depends-on ("package" "lazy"))
               (:file "ensure-parser" :depends-on ("package" "contexts" "basic" "cache"))
               (:file "combinators" :depends-on ("package" "basic" "lazy" "ensure-parser"))
               (:file "primitives" :depends-on ("package" "lazy" "basic" "combinators" "contexts" "cache"))
               (:file "parsers" :depends-on ("package" "basic" "primitives" "combinators" "ensure-parser" "cache"))
               (:file "memoize" :depends-on ("package" "basic" "ensure-parser"))
               (:file "recurse" :depends-on ("package" "basic" "ensure-parser"))
               (:file "greedy" :depends-on ("package" "basic" "primitives" "combinators" "ensure-parser" "cache"))
               (:file "token-parsers" :depends-on ("package" "basic" "primitives" "combinators" "parsers" "greedy"))))
