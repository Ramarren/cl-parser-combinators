(in-package :parser-combinator)

;;; operate on list of tokens

(defclass parser-possibility ()
  ((tree :accessor tree-of :initarg :tree :initform nil)
   (suffix :accessor suffix-of :initarg :suffix :initform nil)))

(defun result (v inp)
  (list (make-instance 'parser-possibility :tree v :suffix inp)))

(defun zero (inp)
  (declare (ignore inp))
  nil)

(defun item (inp)
  (when inp
    (list (make-instance 'parser-possibility :tree (car inp) :suffix (cdr inp)))))

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
  (bind #'item #'(lambda (x)
		   (if (funcall predicate x)
		       (curry #'result x)
		       #'zero))))

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
					     (curry #'result (cons x xs))))))))
    (choice ne-word (curry #'result nil))))