(in-package :parser-combinator)

(defun char? (character)
  (sat (curry #'eql character)))

(defun digit? ()
  (sat #'digit-char-p))

(defun lower? ()
  (sat #'lower-case-p))

(defun upper? ()
  (sat #'upper-case-p))

(defun letter? ()
  (choice (lower?) (upper?)))

(defun alphanum? ()
  (choice (letter?) (digit?)))

(defun word? ()
  (let ((ne-word (bind (letter?)
		       #'(lambda (x)
			   (bind (word?) #'(lambda (xs)
					     (result (cons x xs))))))))
    (choice ne-word (result nil))))

(def-pattern-parser string?
  (() (result nil))
  ((_x . _xs) (mdo (char? _x) (pstring? _xs) (result (cons _x _xs)))))

(def-pattern-parser many?
  (_parser (choice (mdo (<- x _parser) (<- xs (many? _parser)) (result (cons x xs))) (result nil))))

(def-pattern-parser many1?
  (_parser (mdo (<- x _parser) (<- xs (many? _parser)) (result (cons x xs)))))

(defun nat? ()
  (mdo (<- xs (many1? (digit?))) (result (read-from-string (coerce xs 'string)))))

(defun int? ()
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

(defun nat2? ()
  (chainl1 (mdo (<- x (digit?))
		(result (digit-char-p x)))
	   (result
	    #'(lambda (x y)
		(+ (* 10 x) y)))))

(defun chainr1? (p op)
  (bind p #'(lambda (x)
	      (choice
	       (mdo (<- f op)
		    (<- y (chainr1 p op))
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

(defun first? (p);doesn't actually work because there is no laziness, but might free memory at least
  #'(lambda (inp)
      (when-let (results (funcall p inp))
	(list (car results)))))
