(asdf:defsystem parser-combinators-debug
  :version "0"
  :description "A debug extension for PARSER-COMBINATORS"
  :maintainer "Jakub Higersberger <ramarren@gmail.com>"
  :author "Samium Gromoff <_deepfire@feelingofgreen.ru>"
  :licence "BSD-style"
  :depends-on (:parser-combinators :cl-containers)
  :components ((:file "debug")))
