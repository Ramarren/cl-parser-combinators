(in-package :parser-combinator)

(defparameter *memo-table* (make-hash-table))

(defun memoize? (p label)
  "Create identical, but memoized, parser"
  (unless (gethash label *memo-table*)
    (setf (gethash label *memo-table*) (make-hash-table)))
  #'(lambda (inp)
      (let ((memo-table (gethash :label *memo-table*)))
	(multiple-value-bind (result result-p) (gethash inp memo-table)
	  (if result-p
	      (copy-list result)
	      (copy-list (setf (gethash inp memo-table)
			       (funcall p inp))))))))

(defun left-recursive (p alt)
  (let ((count-table (make-hash-table))
	(length-table (make-hash-table)))
    (labels ((recur (inp)
	       (unless (gethash inp length-table)
		 (setf (gethash inp length-table) (length inp)))
	       (incf (gethash inp count-table 0))
	       (if (> (1+ (gethash inp count-table))
		      (gethash inp length-table))
		   (funcall alt inp)
		   (funcall (mdo #'recur p)
			    inp))))
      #'recur)))
