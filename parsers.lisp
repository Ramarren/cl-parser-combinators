(in-package :parser-combinator)

(def-memo1-parser char? character
  (sat (curry #'eql character)))

(def-cached-parser digit?
  (sat #'digit-char-p))

(def-cached-parser lower?
  (sat #'lower-case-p))

(def-cached-parser upper?
  (sat #'upper-case-p))

(def-cached-parser letter?
  (sat #'alpha-char-p))

(def-cached-parser alphanum?
  (sat #'alphanumericp))

(def-cached-parser word?
  (choice (mdo (<- x (letter?)) (<- xs (word?)) (result (cons x xs)))
	  (result nil)))

(def-memo1-parser string? character-list
  (match character-list
    (() (result nil))
    ((_x . _xs) (mdo (char? _x) (string? _xs) (result (cons _x _xs))))))

(defun many? (parser)
  (choice (mdo (<- x parser) (<- xs (many? parser)) (result (cons x xs))) (result nil)))

(defun many1? (parser)
  (mdo (<- x parser) (<- xs (many? parser)) (result (cons x xs))))

(def-cached-parser int?
  (mdo (<- f (choice (mdo (char? #\-) (result #'-)) (result #'identity)))
       (<- n (nat?))
       (result (funcall f n))))

(defun sepby1? (parser-item parser-separator)
  (mdo (<- x parser-item)
       (<- xs (many? (mdo parser-separator (<- y parser-item) (result y))))
       (result (cons x xs))))

(defun bracket? (parser-open parser-center parser-close)
  (mdo parser-open (<- xs parser-center) parser-close (result xs)))

(defun sepby? (parser-item parser-separator)
  (choice (sepby1? parser-item parser-separator) (result nil)))

(defun chainl1? (p op)
  (labels ((rest-chain (x)
	     (choice
	      (mdo (<- f op)
		   (<- y p)
		   (rest-chain (funcall f x y)))
	      (result x))))
    (bind p #'rest-chain)))

(def-cached-parser nat?
  (chainl1? (mdo (<- x (digit?))
		 (result (digit-char-p x)))
	    (result
	     #'(lambda (x y)
		 (+ (* 10 x) y)))))

(defun chainr1? (p op)
  (bind p #'(lambda (x)
	      (choice
	       (mdo (<- f op)
		    (<- y (chainr1? p op))
		    (result (funcall f x y)))
	       (result x)))))

(defun chainl? (p op v)
  (choice
   (chainl1? p op)
   (result v)))

(defun chainr? (p op v)
  (choice
   (chainr1? p op)
   (result v)))
