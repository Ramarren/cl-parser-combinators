(in-package :parser-combinator)

(defparameter *memo-table* (make-hash-table))

(defun memoize? (promise label)
  "Create identical, but memoized, parser promise."
  (unless (gethash label *memo-table*)
    (setf (gethash label *memo-table*) (make-hash-table)))
  (let ((memo-table (gethash :label *memo-table*)))
    #'(lambda ()
	(let ((p (funcall promise)))
	  #'(lambda (inp)
	      (multiple-value-bind (result result-p) (gethash inp memo-table)
		(if result-p
		    (copy-list result)
		    (copy-list (setf (gethash inp memo-table)
				     (funcall p inp))))))))))

(defparameter *curtail-table* (make-hash-table))

(defun curtail? (promise label)
  (unless (gethash label *curtail-table*)
    (setf (gethash label *curtail-table*) (make-hash-table)))
  (let ((curtail-table (gethash label *curtail-table*)))
    #'(lambda ()
	(let ((p (funcall promise)))
	  (labels ((curtailed (inp)
		     (multiple-value-bind (counter counter-p) (gethash inp curtail-table)
		       (cond (counter-p
			      (destructuring-bind (c . l) counter
				(print l)
				(cond ((>= c (1+ l))
				       nil)
				      (t
				       (incf (car counter))
				       (funcall p inp)))))
			     (t
			      (setf (gethash inp curtail-table)
				    (cons 1 (length inp)))
			      (funcall p inp))))))
	    #'curtailed)))))
