(in-package :parser-combinators)

(defun memoize? (parser &optional (label (gensym)))
  "Create identical, but memoized, parser."
  (unless (gethash label *memo-table*)
    (setf (gethash label *memo-table*) (make-hash-table)))
  (let ((memo-table (gethash label *memo-table*)))
    #'(lambda (inp)
        (multiple-value-bind (result result-p) (gethash inp memo-table)
          (let ((new-result (if result-p
                                (copy-parse-result result)
                                (copy-parse-result (setf (gethash inp memo-table)
                                                         (funcall parser inp))))))
            #'(lambda ()
                (next-result new-result)))))))
