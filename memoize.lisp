(in-package :parser-combinators)

(defun memoize? (parser label)
  "Create identical, but memoized, parser."
  (unless (gethash label *memo-table*)
    (setf (gethash label *memo-table*) (make-hash-table)))
  (let ((memo-table (gethash label *memo-table*)))
    #'(lambda (inp)
        (multiple-value-bind (result result-p) (gethash inp memo-table)
          (if result-p
              (copy-list result)
              (copy-list (setf (gethash inp memo-table)
                               (funcall parser inp))))))))
