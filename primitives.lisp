(in-package :parser-combinator)

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
		       :current-result (make-instance 'parser-possibility
						      :tree v :suffix inp)))))

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
			   :current-result (make-instance 'parser-possibility
							  :tree (car inp) :suffix (cdr inp)))
	    (make-instance 'parse-result)))))

(declaim (inline sat))
(defun sat (predicate)
  "Parser: return a token satisfying a predicate."
  (bind (item) #'(lambda (x)
		   (if (funcall predicate x)
		       (result x)
		       (zero)))))

(defun force? (parser)
  "Parser modifier: fully realize result from parser"
  (delay
    #'(lambda (inp)
	(let ((result (funcall parser inp)))
	  (let ((all-results (gather-results result)))
	   (make-instance 'parse-result
			  :current-result (car all-results)
			  :continuation #'(lambda ()
					    (pop all-results))))))))
