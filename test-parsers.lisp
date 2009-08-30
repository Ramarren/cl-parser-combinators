(in-package :parser-combinators-tests)

(in-suite parser-combinators-tests)

(defsuite* parsers-tests)

;;; test for predefined parsers
;;; at least some of them, this is boring, write automatic genetor

(defmacro defparsertest (test-name parser (&rest should-pairs) (&rest fails))
  `(deftest ,test-name ()
     ,@(iter (for (string should) on should-pairs by #'cddr)
             (collect `(is (equal ,should
                                  (tree-of (current-result (parse-string ,parser ,string)))))))
     ,@(iter (for string in fails)
             (collect `(is (null (current-result (parse-string ,parser ,string))))))))

(defparsertest test-char? (char? #\a)
  ("a" #\a)
  ("b"))

(defparsertest test-digit? (digit?)
  ("5" #\5)
  ("b"))

(defparsertest test-lower? (lower?)
  ("a" #\a)
  ("A" "5"))

(defparsertest test-upper? (upper?)
  ("A" #\A)
  ("a" "5"))

(defparsertest test-alphanum? (alphanum?)
  ("A" #\A "b" #\b "1" #\1 "5" #\5)
  (" " ","))

(defparsertest test-word? (word?)
  ("abc" "abc" )
  ("   " "123"))

(defparsertest test-string? (string? (list #\a #\b #\c))
  ("abc" (list #\a #\b #\c))
  ("cde" "abd"))

(defparsertest test-many? (many? (letter?))
  ("abc" (list #\a #\b #\c) "cdef" (list #\c #\d #\e #\f) "Aaa12" (list #\A #\a #\a) "" nil
   "123" nil " ," nil " a" nil)
  ())

(defparsertest test-many1? (many1? (letter?))
  ("abc" (list #\a #\b #\c))
  ("" "123" " a"))

(defparsertest test-times? (times? (letter?) 3)
  ("abc" (list #\a #\b #\c) "abcd" (list #\a #\b #\c))
  ("" "a" "ab"))

(defparsertest test-atleast? (atleast? (letter?) 3)
  ("abc" (list #\a #\b #\c) "abcd" (list #\a #\b #\c #\d))
  ("" "a" "ab"))

(defparsertest test-atmost? (atmost? (letter?) 3)
  ("a" (list #\a) "ab" (list #\a #\b) "abc" (list #\a #\b #\c) "abcd" (list #\a #\b #\c) "" nil "1" nil)
  ())

(defparsertest test-between? (between? (letter?) 2 3)
  ("ab" (list #\a #\b) "abc" (list #\a #\b #\c) "abcd" (list #\a #\b #\c))
  ("a" ""))

(defparsertest test-int? (int?)
  ("5" 5 "12" 12 "145" 145 "-56" -56)
  ("a" "b" " "))

(defparsertest test-sepby1? (sepby1? (int?) (char? #\,))
  ("1,2,3" (list 1 2 3) "4,-5,6" (list 4 -5 6) "23" (list 23))
  ("" "  " "abc"))

(defparsertest test-bracket? (bracket? (char? #\[) (sepby1? (int?) (char? #\,)) (char? #\]))
  ("[1,2,3]" (list 1 2 3) "[4,-5,6]" (list 4 -5 6) "[23]" (list 23))
  ("" "  " "abc" "1,2,3" "[4,5,6"))

(defparsertest test-sepby? (sepby? (int?) (char? #\,))
  ("1,2,3" (list 1 2 3) "4,-5,6" (list 4 -5 6) "23" (list 23) "" nil "abc" nil)
  ())

(defparsertest test-nat? (nat?)
  ("1" 1 "23" 23)
  ("-1"))

(defparsertest test-chainl1? (chainl1? (digit?) (result #'list))
  ("123" (list (list #\1  #\2) #\3))
  (""))

(defparsertest test-chainr1? (chainr1? (digit?) (result #'list))
  ("123" (list #\1 (list #\2 #\3)))
  (""))

(defparsertest test-many* (many* (letter?))
  ("abc" (list #\a #\b #\c) "cdef" (list #\c #\d #\e #\f) "Aaa12" (list #\A #\a #\a) "" nil
   "123" nil " ," nil " a" nil)
  ())

(defparsertest test-many1* (many1* (letter?))
  ("abc" (list #\a #\b #\c))
  ("" "123" " a"))

(defparsertest test-atleast* (atleast* (letter?) 3)
  ("abc" (list #\a #\b #\c) "abcd" (list #\a #\b #\c #\d))
  ("" "a" "ab"))

(defparsertest test-atmost* (atmost* (letter?) 3)
  ("a" (list #\a) "ab" (list #\a #\b) "abc" (list #\a #\b #\c) "abcd" (list #\a #\b #\c) "" nil "1" nil)
  ())

(defparsertest test-between* (between* (letter?) 2 3)
  ("ab" (list #\a #\b) "abc" (list #\a #\b #\c) "abcd" (list #\a #\b #\c))
  ("a" ""))

(defparsertest test-sepby1* (sepby1* (int?) (char? #\,))
  ("1,2,3" (list 1 2 3) "4,-5,6" (list 4 -5 6) "23" (list 23))
  ("" "  " "abc"))

(defparsertest test-sepby* (sepby* (int?) (char? #\,))
  ("1,2,3" (list 1 2 3) "4,-5,6" (list 4 -5 6) "23" (list 23) "" nil "abc" nil)
  ())

(defparsertest test-chainl1* (chainl1* (digit?) (result #'list))
  ("123" (list (list #\1  #\2) #\3))
  (""))

(defparsertest test-chainr1* (chainr1* (digit?) (result #'list))
  ("123" (list #\1 (list #\2 #\3)))
  (""))

(defparsertest test-nat* (nat*)
  ("1" 1 "23" 23)
  ("-1"))

(defparsertest test-int* (int*)
  ("5" 5 "12" 12 "145" 145 "-56" -56)
  ("a" "b" " "))

(deftest test-memo ()
  (is (equal '(("12" "3") ("12") ("1" "23") ("1" "2" "3") ("1" "2") ("1") NIL)
             (mapcar #'tree-of
                     (gather-results
                      (parse-string (many? (between? (memoize? (digit?)) 1 2 'string))
                                    "123"))))))
