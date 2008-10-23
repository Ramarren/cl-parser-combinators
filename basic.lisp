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

;;; lazy results
(defclass parse-result ()
  ((top-results :initform nil :initarg :top-results :accessor top-results-of)
   (promise-list :initform nil :initarg :promise-list :accessor promise-list-of)))

(defun next-result (parse-result)
  (with-accessors ((top-results top-results-of)
		   (promise-list promise-list-of)) parse-result
   (cond ((and (null top-results)
	       (null promise-list))
	  nil)
	 ((and (null top-results)
	       promise-list)
	  (setf top-results (force (pop promise-list)))
	  (next-result parse-result))
	 (top-results
	  (pop top-results)))))

(defun gather-results (parse-result)
  (iter (for result next (next-result parse-result))
	(while result)
	(collect result)))

;;; macros for defining parsers

(defmacro def-cached-parser (name &body body)
  "Define constant parser name. It will we created only once. No parameters."
  (with-unique-names (cache-name)
    (destructuring-bind (docstring body)
	(if (stringp (car body))
	    (list (car body) (cdr body))
	    (list nil body))
      `(progn
	 (defvar ,cache-name)		;to avoid warning about missing functions with self calling
	 (declaim (inline ,name))
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
(declaim (inline result))
(defun result (v)
  "Primitive parser: return v, leaves input unmodified."
  (delay
    #'(lambda (inp)
	(make-instance 'parse-result
		       :top-results (list (make-instance 'parser-possibility
							 :tree v :suffix inp))))))

(def-cached-parser zero
  "Primitive parser: parsing failure"
  (delay
    (constantly (make-instance 'parse-result))))

(def-cached-parser item
  "Primitive parser: consume item from input and return it."
  (delay
    #'(lambda (inp)
	(if inp
	    (make-instance 'parse-result
			   :top-results (list (make-instance 'parser-possibility
							     :tree (car inp) :suffix (cdr inp))))
	    (make-instance 'parse-result)))))

(defun force? (parser)
  "Parser modifier: fully realize result from parser"
  (delay
   #'(lambda (inp)
       (let ((result (funcall parser inp)))
	 (make-instance 'parse-result
			:top-results (gather-results result))))))

;;; emulating monads... did I even understand those?
;;; bind      :: Parser a -> (a -> Parser b) -> Parser b
;;;              (parser-tree1 function-from-tree1-to-parser-tree2)=>parser-tree2
;;; p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]
;;; (bind p f inp)=(concat list-comprehension)

(defun execute-bind (inp parser parser-promise-generator)
  (let ((results-p (funcall parser inp)))
    (assert results-p)
    (iter (for result-p next (next-result results-p))
	  (while result-p)
	  (for v = (tree-of result-p))
	  (for inp-prime = (suffix-of result-p))
	  (for results-q = (funcall (force (funcall parser-promise-generator v)) inp-prime))
	  (nconcing (gather-results results-q)))))

(defmacro bind (parser-promise parser-promise-generator) ; results in parser-promise
  `(delay
     (let ((parser-promise ,parser-promise)
	   (parser-promise-generator ,parser-promise-generator))
       #'(lambda (inp)
	   (make-instance 'parse-result
			  :promise-list
			  (list
			   (delay
			     (execute-bind inp
					   (force parser-promise)
					   parser-promise-generator))))))))

(declaim (inline sat))
(defun sat (predicate)
  "Parser: return a token satisfying a predicate."
  (bind (item) #'(lambda (x)
		   (if (funcall predicate x)
		       (result x)
		       (zero)))))

(defun execute-choice (inp parser1 parser2)
  (let ((result1 (funcall parser1 inp))
	(result2 (funcall parser2 inp)))
    (let ((top1 (top-results-of result1))
	  (top2 (top-results-of result2))
	  (promise1 (promise-list-of result1))
	  (promise2 (promise-list-of result2)))
      (let ((promise-top1 (list (delay top1)))
	    (promise-top2 (list (delay top2))))
	(make-instance 'parse-result
		       :promise-list
		       (append promise-top1 promise1 promise-top2 promise2))))))

(defmacro choice (parser1-promise parser2-promise)
  "Combinator: all alternatives from two parsers"
  `(delay
     (let ((parser1-promise ,parser1-promise)
	   (parser2-promise ,parser2-promise))
       #'(lambda (inp)
	   (execute-choice inp (force parser1-promise) (force parser2-promise))))))

(defmacro choice1 (parser1-promise parser2-promise)
  "Combinator: one alternative from two parsers"
  `(delay
     (let ((parser1-promise ,parser1-promise)
	   (parser2-promise ,parser2-promise))
       #'(lambda (inp)
	   (make-instance 'parse-result
			  :promise-list
			  (list (delay
				  (let ((result1 (next-result (funcall (force parser1-promise) inp))))
				    (if result1
					(make-instance 'parse-result :top-results (list result1))
					(let ((result2 (next-result (funcall (force parser2-promise) inp))))
					  (when result2
					    (list (make-instance 'parse-result :top-results (list result2))))))))))))))

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
	   (make-instance 'parse-result
			  :promise-list
			  (list (delay
				  (iter (for p in parser-promise-list)
					(for result = (next-result (funcall (force p) inp)))
					(finding (make-instance 'parse-result
								:top-results (list (car result-list)))
						 such-that result)))))))))

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
    (funcall (force parser) (coerce string 'list))))
