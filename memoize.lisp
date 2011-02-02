(in-package :parser-combinators)

(defun memoize? (parser &optional (label (gensym)))
  "Parser modifier: memoizes the parser, which if called again at a given position it will return a result
immediately. LABEL is used for memoization key, use explicit one if the parser is used in multiple places."
  (with-parsers (parser)
    #'(lambda (inp)
        (unless (gethash label *memo-table*)
          (setf (gethash label *memo-table*) (make-hash-table)))
        (let ((memo-table (gethash label *memo-table*)))
          (multiple-value-bind (result result-p) (gethash (position-of inp) memo-table)
            (let ((new-result (if result-p
                                  (copy-parse-result result)
                                  (copy-parse-result (setf (gethash (position-of inp) memo-table)
                                                           (make-parse-result (funcall parser inp)))))))
              #'(lambda ()
                  (next-result new-result))))))))
