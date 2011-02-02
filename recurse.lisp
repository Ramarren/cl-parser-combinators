(in-package :parser-combinators)

;; This is a hack, really.
(defvar *curtail* nil)

(defmacro curtail? (name &body body)
  "Parser modifier: add recursion curtailing to PARSER, naming the curtailed parser NAME. Left
recursive parser parser will only be nested once per remaining length of the input string. Note:
this is only necessary for a limited class of left recursive parsers. Non-left recursive parsers
should be implemented using just `named?`, and most left-recursive parsers using that in combination
with `chainl1?`. Also see `expression?`."
  (with-unique-names (inp curtail)
    `(named? ,name
       #'(lambda (,inp)
           (let ((,curtail (if *curtail* *curtail* 0)))
             (if (>= (+ ,curtail (position-of ,inp))
                     (length-of ,inp))
                 (funcall (zero) ,inp)
                 (let ((*curtail* (1+ ,curtail)))
                   (funcall ,@body ,inp))))))))
