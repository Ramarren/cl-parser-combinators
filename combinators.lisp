(in-package :parser-combinator)

;;; emulating monads... did I even understand those?
;;; bind      :: Parser a -> (a -> Parser b) -> Parser b
;;;              (parser-tree1 function-from-tree1-to-parser-tree2)=>parser-tree2
;;; p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]
;;; (bind p f inp)=(concat list-comprehension)

(defun execute-bind (inp parser parser-promise-generator) ;return continuation function
  (let ((p-parse-result (funcall parser inp))
	(q-parse-result nil))
    #'(lambda ()
	(let ((result nil))
	  (iter (when q-parse-result (setf result (next-result q-parse-result)))
		(unless result
		  (setf q-parse-result
			(let ((p-next-result
			       (next-result p-parse-result)))
			  (if p-next-result
			      (let ((v (tree-of p-next-result))
				    (inp-prime (suffix-of p-next-result)))
				(funcall (force (funcall parser-promise-generator v)) inp-prime))
			      (setf p-parse-result nil)))))
		(until (or result
			   (and (null p-parse-result)
				(null q-parse-result)))))
	  result))))

(defmacro bind (parser-promise parser-promise-generator) ; results in parser-promise
  `(delay
     (let ((parser-promise ,parser-promise)
	   (parser-promise-generator ,parser-promise-generator))
       #'(lambda (inp)
	   (make-instance 'parse-result
			  :continuation
			  (execute-bind inp
					(force parser-promise)
					parser-promise-generator))))))

(defun execute-choice (inp parser1 parser2)
  (let ((result1 (funcall parser1 inp))
	(result2 nil))
    #'(lambda ()
	(cond (result1
	       (let ((result (next-result result1)))
		 (unless result
		   (setf result1 nil)
		   (setf result2 (funcall parser2 inp))
		   (setf result (next-result result2)))
		 result))
	      (result2
	       (let ((result (next-result result2)))
		 (unless result
		   (setf result2 nil)
		   result)))
	      (t nil)))))

(defmacro choice (parser1-promise parser2-promise)
  "Combinator: all alternatives from two parsers"
  `(delay
     (let ((parser1-promise ,parser1-promise)
	   (parser2-promise ,parser2-promise))
       #'(lambda (inp)
	   (make-instance 'parse-result
			  :continuation
			  (execute-choice inp (force parser1-promise) (force parser2-promise)))))))

(defmacro choice1 (parser1-promise parser2-promise)
  "Combinator: one alternative from two parsers"
  `(delay
     (let ((parser1-promise ,parser1-promise)
	   (parser2-promise ,parser2-promise)
	   (is-unread t))
       #'(lambda (inp)
	   (make-instance 'parse-result
			  :continuation
			  #'(lambda ()
			      (when is-unread
				(setf is-unread nil)
				(let ((result (funcall (execute-choice inp
								       (force parser1-promise)
								       (force parser2-promise)))))
				  (setf parser1-promise nil parser2-promise nil)
				  result))))))))

(defmacro choices (&rest parser-promise-list)
  "Combinator: all alternatives from multiple parsers"
  (if (cdr parser-promise-list)
      `(choice ,(car parser-promise-list)
	       (choices ,@(cdr parser-promise-list)))
      (car parser-promise-list)))

(defmacro choices1 (&rest parser-promise-list)
  "Combinator: one alternative from multiple parsers"
  `(delay
     (let ((parser-promise-list (list ,@parser-promise-list))
	   (is-unread t))
       #'(lambda (inp)
	   (make-instance 'parse-result
			  :continuation
			  #'(lambda ()
			      (when is-unread
				(setf is-unread t)
				(iter (for p in parser-promise-list)
				      (for result = (next-result (funcall (force p) inp)))
				      (finding (make-instance 'parse-result
							      :top-results (list (car result-list)))
					       such-that result)
				      (finally (setf parser-promise-list nil))))))))))
