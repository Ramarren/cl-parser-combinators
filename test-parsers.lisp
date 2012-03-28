(in-package :parser-combinators-tests)

(defsuite* (parsers-tests :in parser-combinators-tests))

;;; test for predefined parsers
;;; at least some of them, this is boring, write automatic genetor

(defmacro defparsertest (test-name parser (&rest should-pairs) (&rest fails))
  (with-gensyms (result)
    `(deftest ,test-name ()
       ,@(iter (for (string should) on should-pairs by #'cddr)
           (collect `(let ((,result (current-result (parse-string ,parser ,string))))
                       (when (is ,result "Failed to parse ~s" ,string)
                         (is (equal ,should
                                    (tree-of ,result)))))))
       ,@(iter (for string in fails)
           (collect `(is (null (current-result (parse-string ,parser ,string)))))))))

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

(defparsertest test-whitespace? (whitespace?)
  ("   " nil " " nil)
  ("a" ","))

(defparsertest test-whitespace* (whitespace*)
  ("   " nil " " nil)
  ("a" ","))

(defparsertest test-whitespace?2 (whitespace? :accept-empty t)
  ("" nil)
  ())

(defparsertest test-whitespace*2 (whitespace* :accept-empty t)
  ("" nil)
  ())

(defparsertest test-word? (word?)
  ("abc" "abc" "123" "123" "abc!def" "abc")
  ("   " ";,!"))

(defparsertest test-word* (word*)
  ("abc" "abc" "123" "123" "abc!def" "abc")
  ("   " ";,!"))

(defparsertest test-pure-word? (pure-word?)
  ("abc" "abc" "abc!def" "abc")
  ("   " ";,!" "123"))

(defparsertest test-pure-word* (pure-word*)
  ("abc" "abc" "abc!def" "abc")
  ("   " ";,!" "123"))

(defparsertest test-string? (string? (list #\a #\b #\c) :result-type 'list)
  ("abc" (list #\a #\b #\c) "abcd" (list #\a #\b #\c))
  ("cde" "abd" "aBc"))

(defparsertest test-string?-chain (seq-list? "abc" "def")
  ("abcdef" (list "abc" "def") "abcdefgh" (list "abc" "def"))
  ("abcedef"))

(deftest test-string?-endcontext ()
  (is (null (nth-value 1 (parse-string* "abc" "abc")))))

(defparsertest test-string?-char-equal (string? (list #\a #\b #\c) :test #'char-equal :result-type 'string)
  ("aBc" "aBc" "ABC" "ABC" "abCd" "abC")
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

(defparsertest test-nat?2 (seq-list? (nat?) "15")
  ("1015" '(10 "15"))
  ())

(defparsertest test-chainl1? (chainl1? (digit?) (result #'list))
  ("123" (list (list #\1  #\2) #\3))
  (""))

(defparsertest test-chainr1? (chainr1? (digit?) (result #'list))
  ("123" (list #\1 (list #\2 #\3)))
  (""))

(defparsertest test-find? (find? "ab")
  ("cacbcacab" "ab")
  ("" "cacacbac"))

(defparsertest test-find?-context (seq-list? (find? "aa") "bbaacc")
  ("aabbaacc" (list "aa" "bbaacc"))
  ())

(defparsertest test-find* (find* "ab")
  ("cacbcacab" "ab")
  ("" "cacacbac"))

(defparsertest test-find*-context (seq-list* (find* "aa") "bbaacc")
  ("aabbaacc" (list "aa" "bbaacc"))
  ())

(defparsertest test-find-after? (find-after? #\c "ab")
  ("ccccccab" "ab" "cab" "ab")
  ("" "acb" "bcccab"))

(defparsertest test-find-after-collect? (find-after-collect? #\a "cc" 'string)
  ("aaaacc" '("aaaa" . "cc"))
  ("aaaa" "aaac"))

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

(defparsertest test-find-after* (find-after* "ab" "cd")
  ("abababcd" "cd" "cd" "cd")
  ("ababab" "adbacd"))

(defparsertest test-gather-before-token* (gather-before-token* #\a :result-type 'string :accept-end t)
  ("cccddda" "cccddd" "gthj" "gthj")
  ())

(defparsertest test-gather-before-token*-chain
    (seq-list* (gather-before-token* #\a :result-type 'string :accept-end t)
               (gather-before-token* #\b :result-type 'string :accept-end t :accept-empty t))
  ("cccdddaghb" (list "cccddd" "agh") "gthj" (list "gthj" ""))
  ())

(defparsertest test-find-before-token* (find-before-token* "ab" #\c)
  ("abababc" (list "ab" "ab" "ab"))
  ("def"))

(deftest test-memo ()
  (is (equal '(("12" "3") ("12") ("1" "23") ("1" "2" "3") ("1" "2") ("1") NIL)
             (mapcar #'tree-of
                     (gather-results
                      (parse-string (many? (between? (memoize? (digit?)) 1 2 'string))
                                    "123"))))))

(deftest test-context ()
  (is (equal "1234"
             (tree-of (current-result
                       (parse-string (mdo (<- c1 (context?))
                                          (many* (digit?))
                                          (<- c2 (context?))
                                          (result (context-interval c1 c2)))
                                     "1234abc")))))
  (is (equal "1234"
             (tree-of (current-result
                       (parse-string (mdo (<- c1 (context?))
                                          (many* (digit?))
                                          (<- c2 (context?))
                                          (result (context-interval c1 c2)))
                                     "1234"))))))

(defparsertest test-mdo-constants (mdo (times? "ab" 2)
                                       #\c
                                       "defg")
  ("ababcdefg" "defg")
  ("ababccdefg"))

(defparsertest test-find-before? (mdo (<- word (word?))
                                      (<- term #\;)
                                      (result (list term word)))
  ("alamakota;" '(#\; "alamakota"))
  ("alamakota" ";"))

(defparsertest test-find-before* (mdo (<- word (word?))
                                      (<- term #\;)
                                      (result (list term word)))
  ("alamakota;" '(#\; "alamakota"))
  ("alamakota" ";"))

(defparsertest test-end? (mdo (<- word (word?))
                              (<- end (end?))
                              (result (list word end)))
  ("alamakota" '("alamakota" t))
  ("alamakota " ""))

(defparsertest test-empty? (end?)
  ("" t)
  ("abc"))

(defparsertest test-seq-list? (seq-list? #\a #\b #\c)
  ("abc" '(#\a #\b #\c))
  ("abd"))

(defparsertest test-named-seq? (named-seq? (<- a #\a) #\b (<- c #\c)
                                           (list a 'b c))
  ("abc" '(#\a b #\c))
  ("acb"))

(defparsertest test-seq-list* (seq-list* #\a #\b #\c)
  ("abc" '(#\a #\b #\c))
  ("abd"))

(defparsertest test-named-seq* (named-seq* (<- a #\a) #\b (<- c #\c)
                                           (list a 'b c))
  ("abc" '(#\a b #\c))
  ("acb"))

(defparsertest test-hook? (hook? #'1+ (nat*))
  ("5" 6 "28" 29)
  ("aa"))

(defparsertest test-chook? (chook? 5 "abc")
  ("abc" 5)
  ("bac"))

(defparsertest test-validate? (validate? (many* (item)) (curry #'length= 5))
  ("12345" '(#\1 #\2 #\3 #\4 #\5))
  ("1234" "123456"))

(defparsertest test-except? (except? (many* (item)) "a")
    ("123" '(#\1 #\2 #\3) "cba" '(#\c #\b #\a))
    ("abc"))

(defparsertest test-except?2 (except? (gather-before-token* #\; :accept-empty t :accept-end t) (end?))
    ("123;" '(#\1 #\2 #\3) ";" nil "123" '(#\1 #\2 #\3))
    (""))

(defparsertest test-opt? (named-seq? (<- a "a") (<- b (opt? "b")) (<- c "c") (list a b c))
  ("abc" '(#\a #\b #\c) "ac" '(#\a nil #\c))
  ("abbc" "aabc"))

(defparsertest test-opt* (named-seq* (<- a "a") (<- b (opt* "b")) (<- c "c") (list a b c))
  ("abc" '(#\a #\b #\c) "ac" '(#\a nil #\c))
  ("abbc" "aabc"))

(defparsertest test-nested? (nested? "A" :min 1)
  ("AA(A(AA)A)A" '(#\A #\A (#\A (#\A #\A) #\A) #\A)
   "AAAA" '(#\A #\A #\A #\A))
  ("BAF"))

(defparsertest test-curtail? (curtail? e (choice (seq-list? e #\+ e) #\A))
  ("A" #\A "A+A" '(#\A #\+ #\A) "A+A+A" '((#\A #\+ #\A) #\+ #\A))
  ())

(defparsertest test-quoted? (quoted? :quote-char #\' :escape-char #\- :include-quotes t)
  ("'AAA'" "'AAA'" "'AB-'C'" "'AB-'C'")
  ("AAA"))

(defparsertest test-quoted?2 (quoted? :quote-char #\' :escape-char #\- :include-quotes nil)
  ("'AAA'" "AAA" "'AB-'C'" "AB'C")
  ("AAA"))

(defparsertest test-quoted?3 (quoted? :left-quote-char #\1 :right-quote-char #\2 :escape-char #\3 :include-quotes nil)
  ("1AAA2" "AAA" "1AB32C2" "AB2C")
  ("AAA"))

