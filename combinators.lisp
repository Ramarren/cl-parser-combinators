(in-package :parser-combinator)

;;; emulating monads... did I even understand those?
;;; bind      :: Parser a -> (a -> Parser b) -> Parser b
;;;              (parser-tree1 function-from-tree1-to-parser-tree2)=>parser-tree2
;;; p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]
;;; (bind p f inp)=(concat list-comprehension)

(defun execute-bind (inp parser parser-promise-generator) ;return a list of promises
  (let ((results-p (funcall parser inp)))
    (let ((top-p (top-results-of results-p))
	  (promises-p (promise-list-of results-p)))
      (let ((top-promise-p (when top-p (delay top-p))))
	(let ((promises (if top-promise-p (cons top-promise-p promises-p) promises-p)))
	  (iter (for promise in promises)
		(collecting
		 (delay (iter (for result in (force promise))
			      (for v = (tree-of result))
			      (for inp-prime = (suffix-of result))
			      (for results-q = (funcall (force (funcall parser-promise-generator v)) inp-prime))
			      (nconcing (gather-results results-q)))))))))))

(defmacro bind (parser-promise parser-promise-generator) ; results in parser-promise
  `(delay
     (let ((parser-promise ,parser-promise)
	   (parser-promise-generator ,parser-promise-generator))
       #'(lambda (inp)
	   (make-instance 'parse-result
			  :promise-list
			  (execute-bind inp
					(force parser-promise)
					parser-promise-generator))))))

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
