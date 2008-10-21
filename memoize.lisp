(in-package :parser-combinator)

(defun memoize? (p)
  "Create identical, but memoized, parser"
  (let ((memo-table (make-hash-table)))
    #'(lambda (inp)
	(let ((result (gethash inp memo-table :first-time-called)))
	  (if (eql result :first-time-called)
	      (copy-list (setf (gethash inp memo-table)
			       (funcall p inp)))
	      (copy-list result))))))