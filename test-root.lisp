(defpackage :parser-combinators-tests (:use :cl :hu.dwim.stefil :iterate :parser-combinators :alexandria :bpm))

(in-package :parser-combinators-tests)

(defsuite* (parser-combinators-tests :in root-suite))
