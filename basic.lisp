(in-package :parser-combinator)

;;; operate on list of tokens

(defclass parser-possibility ()
  ((tree :accessor tree-of :initarg :tree :initform nil)
   (suffix :accessor suffix-of :initarg :suffix :initform nil)))

(defun result (v)
  #'(lambda (inp)
      (list (make-instance 'parser-possibility :tree v :suffix inp))))

(defun zero ()
  (constantly nil))

(defun item ()
  #'(lambda (inp)
      (when inp
	(list (make-instance 'parser-possibility :tree (car inp) :suffix (cdr inp))))))

;;; emulating monads... did I even understand those?
;;; bind      :: Parser a -> (a -> Parser b) -> Parser b
;;;              (parser-tree1 function-from-tree1-to-parser-tree2)=>parser-tree2
;;; p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]
;;; (bind p f inp)=(concat list-comprehension)


(defun bind (parser parser-generator)
  #'(lambda (inp)
      (iter (for possibility in (funcall parser inp))
	    (for v = (tree-of possibility))
	    (for inp-prime = (suffix-of possibility))
	    (nconcing (funcall (funcall parser-generator v) inp-prime)))))

(defun sat (predicate)
  (bind (item) #'(lambda (x)
		   (if (funcall predicate x)
		       (result x)
		       (zero)))))

(defun char? (character)
  (sat (curry #'eql character)))

(defun digit? ()
  (sat #'digit-char-p))

(defun lower? ()
  (sat #'lower-case-p))

(defun upper? ()
  (sat #'upper-case-p))

(defun choice (parser1 parser2)
  #'(lambda (inp)
      (nconc (funcall parser1 inp)
	     (funcall parser2 inp))))

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

(defun bind-comprehension (input &rest parser-spec)
  (cond ((null parser-spec)
	 `(result ,input))
	(t (destructuring-bind (variable parser . remain) parser-spec
	     `(bind ,parser
		    #'(lambda (,variable)
			,@(when (eql variable '_)
			    (list `(declare (ignore ,variable))))
			,(apply #'bind-comprehension input remain)))))))

;;; spec as in monad comprehensions, result variable parser variable parser...
(defmacro defparser (name input &body parser-spec)
  `(defun ,name (,input)
     ,(apply #'bind-comprehension input parser-spec)))

(defparser %string? character-list
  _ (char? (car character-list))
  _ (string? (cdr character-list)))

(defun string? (character-list)
  (let ((string-parser (%string? character-list))
	(result-nil (result nil)))
    (if (null character-list)
	result-nil
	string-parser)))

;;; here parser spec is list of (pattern optional-guard comprehension)
;;; using do-like notation, <- is special

;;; list of either monads: (monad parameters), name bindings (<- name monad)
;;; simple, no let

(defun do-notation (monad-sequence bind)
  (match monad-sequence
    ((_monad . nil)
     _monad)
    (((<- _name _monad) . _)
     `(,bind ,_monad
	     #'(lambda (,_name)
		 ,(do-notation (cdr monad-sequence) bind))))
    ((_monad . _)
     `(,bind ,_monad
	     #'(lambda (_)
		 (declare (ignore _))
		 ,(do-notation (cdr monad-sequence) bind))))))

(defmacro mdo (&body spec)
  (do-notation spec 'bind))

(defmacro def-pattern-parser (name &body parser-patterns)
  (with-unique-names (parameter)
    `(defun ,name (,parameter)
       (match ,parameter
	 ,@(iter (for spec in parser-patterns)
		 (collect
		     (match spec
		       ((_pattern (where _guard) . _spec)
			(list* _pattern (where _guard) _spec))
		       ((_pattern (where-not _guard) . _spec)
			(list* _pattern (where-not _guard) _spec))
		       ((_pattern . _spec)
			(list* _pattern _spec))
		       (_ (error "Error when constructing parser ~a" name)))))))))

(def-pattern-parser psat
  (_predicate (mdo (<- x (item)) (if (funcall _predicate x) (result x) (zero)))))

(def-pattern-parser pstring?
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
