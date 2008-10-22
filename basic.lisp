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

(defun choice (parser1 parser2)
  #'(lambda (inp)
      (nconc (funcall parser1 inp)
	     (funcall parser2 inp))))

(defun choice1 (parser1 parser2)
  #'(lambda (inp)
      (let ((results1 (funcall parser1 inp)))
	(if results1
	    (list (car results1))
	    (let ((results2 (funcall parser2 inp)))
	      (when results2
		(list (car results2))))))))

(defun choices (&rest parser-list)
  (if (cdr parser-list)
      (choice (car parser-list)
	      (apply #'choices (cdr parser-list)))
      (car parser-list)))

(defun choices1 (&rest parser-list)
  #'(lambda (inp)
      (iter (for p in parser-list)
	    (for result-list = (funcall p inp))
	    (finding (list (car result-list)) such-that result-list))))

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

(defun parse-string (parser string)
  (mapcar #'tree-of (funcall parser (coerce string 'list))))
