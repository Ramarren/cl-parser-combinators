(in-package :parser-combinator)

;;; operate on list of tokens

(defclass parser-possibility ()
  ((tree :accessor tree-of :initarg :tree :initform nil)
   (suffix :accessor suffix-of :initarg :suffix :initform nil)))


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
