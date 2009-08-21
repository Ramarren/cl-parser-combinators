(in-package :parser-combinators)

(def-cached-arg-parser char? (character)
  "Parser: accept token eql to argument"
  (sat (curry #'eql character)))

(def-cached-parser digit?
  "Parser: accept digit character"
  (sat #'digit-char-p))

(def-cached-parser lower?
  "Parser: accept lowercase character"
  (sat #'lower-case-p))

(def-cached-parser upper?
  "Parser: accept uppercase character"
  (sat #'upper-case-p))

(def-cached-parser letter?
  "Parser: accept alphabetic character"
  (sat #'alpha-char-p))

(def-cached-parser alphanum?
  "Parser: accept alphanumeric character"
  (sat #'alphanumericp))

(def-cached-parser word?
  "Parser: accept a string of alphabetic characters"
  (choice (mdo (<- x (letter?)) (<- xs (word?)) (result (cons x xs)))
          (result nil)))

;;; all repetition parsers return result as list

(defun many? (parser)
  "Parser: accept zero or more repetitions of expression accepted by parser"
  (choice (mdo (<- x parser) (<- xs (many? parser)) (result (cons x xs))) (result nil)))

(defun many1? (parser)
  "Parser: accept one or more of expression accepted by parser"
  (mdo (<- x parser) (<- xs (many? parser)) (result (cons x xs))))

(defun times? (parser count)
  "Parser: accept exactly count expressions accepted by parser"
  (if (zerop count)
      (result nil)
      (mdo (<- x parser) (<- xs (times? parser (1- count))) (result (cons x xs)))))

(defun atleast? (parser count)
  "Parser: accept at least count expressions accepted by parser"
  (if (zerop count)
      (many? parser)
      (mdo (<- x parser) (<- xs (atleast? parser (1- count))) (result (cons x xs)))))

(defun atmost? (parser count)
  "Parser: accept at most count expressions accepted by parser"
  (if (zerop count)
      (result nil)
      (choice (mdo (<- x parser) (<- xs (atmost? parser (1- count))) (result (cons x xs))) (result nil))))

(defun between? (parser min max)
  "Parser: accept between min and max expressions accepted by parser"
  (assert (>= max min))
  (if (zerop min)
      (atmost? parser max)
      (mdo (<- x parser) (<- xs (between? parser (1- min) (1- max))) (result (cons x xs)))))

(defun int? ()
  "Parser: accept and integer"
  (mdo (<- f (choice (mdo (char? #\-) (result #'-)) (result #'identity)))
       (<- n (nat?))
       (result (funcall f n))))

(defun sepby1? (parser-item parser-separator)
  "Parser: accept at least one of parser-item separated by parser-separator"
  (mdo (<- x parser-item)
       (<- xs (many? (mdo parser-separator (<- y parser-item) (result y))))
       (result (cons x xs))))

(defun bracket? (parser-open parser-center parser-close)
  "Parser: accept parser-center bracketed by parser-open and parser-close"
  (mdo parser-open (<- xs parser-center) parser-close (result xs)))

(defun sepby? (parser-item parser-separator)
  "Parser: accept zero or more of parser-item separated by parser-separator"
  (choice (sepby1? parser-item parser-separator) (result nil)))

(defun chainl1? (p op)
  "Parser: accept one or more p reduced by result of op with left associativity"
  (labels ((rest-chain (x)
             (choice
              (mdo (<- f op)
                   (<- y p)
                   (rest-chain (funcall f x y)))
              (result x))))
    (bind p #'rest-chain)))

(defun nat? ()
  "Parser: accept natural numbers"
  (chainl1? (mdo (<- x (digit?))
                 (result (digit-char-p x)))
            (result
             #'(lambda (x y)
                 (+ (* 10 x) y)))))

(defun chainr1? (p op)
  "Parser: accept one or more p reduced by result of op with right associativity"
  (bind p #'(lambda (x)
              (choice
               (mdo (<- f op)
                    (<- y (chainr1? p op))
                    (result (funcall f x y)))
               (result x)))))

(defun chainl? (p op v)
  "Parser: like chainl1?, but will return v if no p can be parsed"
  (choice
   (chainl1? p op)
   (result v)))

(defun chainr? (p op v)
  "Parser: like chainr1?, but will return v if no p can be parsed"
  (choice
   (chainr1? p op)
   (result v)))
