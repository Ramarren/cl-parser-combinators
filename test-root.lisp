(defpackage :parser-combinators-tests (:use :cl :hu.dwim.stefil :iterate :parser-combinators :alexandria)
  (:import-from :fare-matcher #:match #:like-when))

(in-package :parser-combinators-tests)

(defsuite* (parser-combinators-tests :in root-suite))
