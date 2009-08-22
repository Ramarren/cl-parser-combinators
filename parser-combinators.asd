(asdf:defsystem parser-combinators
  :version "0"
  :description "Toy implementation of parser combinators"
  :maintainer "Jakub Higersberger <ramarren@gmail.com>"
  :author "Jakub Higersberger <ramarren@gmail.com>"
  :licence "BSD-style"
  :depends-on (:iterate :alexandria :bpm)
  :components ((:file "package")
               (:file "queue" :depends-on ("package"))
               (:file "contexts" :depends-on ("package"))
               (:file "lazy" :depends-on ("package"))
               (:file "basic" :depends-on ("package" "lazy"))
               (:file "combinators" :depends-on ("package" "basic" "lazy"))
               (:file "primitives" :depends-on ("package" "lazy" "basic" "combinators" "contexts"))
               (:file "parsers" :depends-on ("package" "basic" "primitives" "combinators"))
               (:file "memoize" :depends-on ("package" "basic"))
               (:file "recurse" :depends-on ("package" "basic"))
               (:file "greedy" :depends-on ("package" "basic" "primitives" "combinators"))))
