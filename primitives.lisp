(in-package :parser-combinator)

;;; macros for defining parsers

(defparameter *parser-cache* (make-hash-table))

(defmacro def-cached-parser (name &body body)
  "Define constant parser name. It will we created only once. No parameters."
  (with-unique-names (cache-name)
    (destructuring-bind (docstring body)
	(if (stringp (car body))
	    (list (car body) (cdr body))
	    (list nil body))
      `(progn
	 (setf (gethash ',name *parser-cache*)
	       (progn ,@body))
	 (declaim (inline ,name))
	 (defun ,name ()
	   ,@(list docstring)
	   (gethash ',name *parser-cache*))))))

(defmacro def-memo1-parser (name argument &body body)
  "Define memoized parser parametrized by one argument, which should be equal under equal."
  (with-unique-names (cache-table cache)
    (destructuring-bind (docstring body)
	(if (stringp (car body))
	    (list (car body) (cdr body))
	    (list nil body))
      `(progn
	 (setf (gethash ',name *parser-cache*) (make-hash-table :test 'equal))
	 (defun ,name (,argument)
	   ,@(list docstring)
	   (let ((,cache-table (gethash ',name *parser-cache*)))
	     (let ((,cache (gethash ,argument ,cache-table)))
	       (if ,cache ,cache (setf (gethash ,argument ,cache-table)
				       (progn ,@body))))))))))

(defmacro def-memo-parser (name argument-list &body body)
  "Define memoized parser parametrized by one argument, which should be equal under equal."
  (with-unique-names (cache-table cache)
    (destructuring-bind (docstring body)
	(if (stringp (car body))
	    (list (car body) (cdr body))
	    (list nil body))
      `(progn
	 (setf (gethash ',name *parser-cache*) (make-hash-table :test 'equal))
	 (defun ,name (,@argument-list)
	   ,@(list docstring)
	   (let ((cache-table (gethash ',name *parser-cache*)))
	     (let ((,cache (gethash (list ,@argument-list) ,cache-table)))
	       (if ,cache ,cache (setf (gethash (list ,@argument-list) ,cache-table)
				       (progn ,@body))))))))))

;;; primitive parsers
(declaim (inline result))
(defun result (v)
  "Primitive parser: return v, leaves input unmodified."
  (delay
    #'(lambda (inp)
	(let ((closure-value (make-instance 'parser-possibility
					    :tree v :suffix inp)))
	  (make-instance 'parse-result
			 :continuation #'(lambda ()
					   (when closure-value
					     (prog1
						 closure-value
					       (setf closure-value nil)))))))))

(def-cached-parser zero
  "Primitive parser: parsing failure"
  (delay
    (constantly (make-instance 'parse-result))))

(def-cached-parser item
  "Primitive parser: consume item from input and return it."
  (delay
    #'(lambda (inp)
	(let ((closure-value (make-instance 'parser-possibility
					    :tree (car inp) :suffix (cdr inp))))
	  (if inp
	      (make-instance 'parse-result
			     :continuation #'(lambda ()
					       (when closure-value
						 (prog1
						     closure-value
						   (setf closure-value nil)))))
	      (make-instance 'parse-result))))))

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
