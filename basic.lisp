(in-package :parser-combinator)

;;; operate on list of tokens

(defclass parser-possibility ()
  ((tree :accessor tree-of :initarg :tree :initform nil)
   (suffix :accessor suffix-of :initarg :suffix :initform nil)))

;;; emulate laziness as well, otherwhise any sort of recursion fails hard
(defclass promise ()
  ((thunk :accessor thunk-of :initarg :thunk)
   (value :accessor value-of :initform nil)))

(defmacro delay (&body body)
  `(make-instance 'promise
		  :thunk #'(lambda ()
			     ,@body)))

(defun force (promise)
  (with-accessors ((value value-of) (thunk thunk-of)) promise
    (if thunk
	(let ((real-value (funcall thunk)))
	  (setf value real-value
		thunk nil)
	  real-value)
	value)))

(defmacro def-cached-parser (name &body body)
  "Define constant parser name. It will we created only once. No parameters."
  (with-unique-names (cache-name)
    (destructuring-bind (docstring body)
	(if (stringp (car body))
	    (list (car body) (cdr body))
	    (list nil body))
      `(progn
	 (defvar ,cache-name)		;to avoid warning about missing functions with self calling
	 (defun ,name ()
	   ,@(list docstring)
	   ,cache-name)
	 (setf ,cache-name (progn ,@body))))))

(defmacro def-memo1-parser (name argument &body body)
  "Define memoized parser parametrized by one argument, which should be equal under equal."
  (with-unique-names (cache-table-name cache)
    (destructuring-bind (docstring body)
	(if (stringp (car body))
	    (list (car body) (cdr body))
	    (list nil body))
      `(progn
	 (defparameter ,cache-table-name (make-hash-table :test 'equal))
	 (defun ,name (,argument)
	   ,@(list docstring)
	   (let ((,cache (gethash ,argument ,cache-table-name)))
	     (if ,cache ,cache (setf (gethash ,argument ,cache-table-name)
				     (progn ,@body)))))))))

(defmacro def-memo-parser (name argument-list &body body)
  "Define memoized parser parametrized by one argument, which should be equal under equal."
  (with-unique-names (cache-table-name cache)
    (destructuring-bind (docstring body)
	(if (stringp (car body))
	    (list (car body) (cdr body))
	    (list nil body))
      `(progn
	 (defparameter ,cache-table-name (make-hash-table :test 'equal))
	 (defun ,name (,@argument-list)
	   ,@(list docstring)
	   (let ((,cache (gethash (list ,@argument-list) ,cache-table-name)))
	     (if ,cache ,cache (setf (gethash (list ,@argument-list) ,cache-table-name)
				     (progn ,@body)))))))))

;;; primitive parsers

(defun result (v)
  "Primitive parser: return v, leaves input unmodified."
  (delay
    #'(lambda (inp)
	(list (make-instance 'parser-possibility :tree v :suffix inp)))))

(def-cached-parser zero
  "Primitive parser: parsing failure"
  (delay
    (constantly nil)))

(def-cached-parser item
  "Primitive parser: consume item from input and return it."
  (delay
    #'(lambda (inp)
	(when inp
	  (list (make-instance 'parser-possibility :tree (car inp) :suffix (cdr inp)))))))

;;; emulating monads... did I even understand those?
;;; bind      :: Parser a -> (a -> Parser b) -> Parser b
;;;              (parser-tree1 function-from-tree1-to-parser-tree2)=>parser-tree2
;;; p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]
;;; (bind p f inp)=(concat list-comprehension)

(defmacro bind (parser-promise parser-promise-generator) ; results in parser-promise
  `(delay
     (let ((parser-promise ,parser-promise)
	   (parser-promise-generator ,parser-promise-generator))
       #'(lambda (inp)
	   (iter (for possibility in (funcall (force parser-promise) inp))
		 (for v = (tree-of possibility))
		 (for inp-prime = (suffix-of possibility))
		 (nconcing (funcall (force (funcall parser-promise-generator v)) inp-prime)))))))

(defun sat (predicate)
  "Parser: return a token satisfying a predicate."
  (bind (item) #'(lambda (x)
		   (if (funcall predicate x)
		       (result x)
		       (zero)))))

(defmacro choice (parser1-promise parser2-promise)
  "Combinator: all alternatives from two parsers"
  `(delay
     (let ((parser1-promise ,parser1-promise)
	   (parser2-promise ,parser2-promise))
       #'(lambda (inp)
	   (nconc (funcall (force parser1-promise) inp)
		  (funcall (force parser2-promise) inp))))))

(defmacro choice1 (parser1-promise parser2-promise)
  "Combinator: one alternative from two parsers"
  `(delay
     (let ((parser1-promise ,parser1-promise)
	   (parser2-promise ,parser2-promise))
       #'(lambda (inp)
	   (let ((results1 (funcall (force parser1-promise) inp)))
	     (if results1
		 (list (car results1))
		 (let ((results2 (funcall (force parser2-promise) inp)))
		   (when results2
		     (list (car results2))))))))))

(defmacro choices (&rest parser-promise-list)
  "Combinator: all alternatives from multiple parsers"
  (if (cdr parser-promise-list)
      `(choice ,(car parser-promise-list)
	       (choices ,@(cdr parser-promise-list)))
      (car parser-promise-list)))

(defmacro choices1 (&rest parser-promise-list)
  "Combinator: one alternative from multiple parsers"
  `(delay
     (let ((parser-promise-list (list ,@parser-promise-list)))
       #'(lambda (inp)
	   (iter (for p in parser-promise-list)
		 (for result-list = (funcall (force p) inp))
		 (finding (list (car result-list)) such-that result-list))))))

;;; here parser spec is list of (pattern optional-guard comprehension)
;;; using do-like notation, <- is special

;;; list of either monads: (monad parameters), name bindings (<- name monad)
;;; simple, no let

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-notation (monad-sequence bind ignore-gensym)
    (match monad-sequence
      ((_monad . nil)
       _monad)
      (((<- _name _monad) . _)
       `(,bind ,_monad
	       #'(lambda (,_name)
		   ,(do-notation (cdr monad-sequence) bind ignore-gensym))))
      ((_monad . _)
       `(,bind ,_monad
	       #'(lambda (,ignore-gensym)
		   (declare (ignore ,ignore-gensym))
		   ,(do-notation (cdr monad-sequence) bind ignore-gensym)))))))

(defmacro mdo (&body spec)
  "Combinator: use do-like notation to sequentially link parsers. (<- name parser) allows capturing return values, last form must be (result form)."
  (with-unique-names (ignore-gensym)
    (do-notation spec 'bind ignore-gensym)))

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

(defparameter *curtail-table* (make-hash-table))
(defparameter *memo-table* (make-hash-table))

(defun parse-string (parser string)
  "Parse a string, return list of possible parse trees. Return remaining suffixes as second value. All returned values may share structure."
  (let ((*memo-table* (make-hash-table))
	(*curtail-table* (make-hash-table)))
    (let ((results (funcall (force parser) (coerce string 'list))))
     (values (mapcar #'tree-of results)
	     (mapcar #'suffix-of results)))))
