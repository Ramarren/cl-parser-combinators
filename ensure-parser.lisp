(in-package :parser-combinators)

(declaim (inline sat))
(def-cached-arg-parser sat (predicate)
  "Parser: return a token satisfying a predicate."
  #'(lambda (inp)
      (typecase inp
        (end-context (constantly nil))
        (context
           (if (funcall predicate (context-peek inp))
               (let ((closure-value
                      (make-instance 'parser-possibility
                                     :tree (context-peek inp) :suffix (context-next inp))))
                 #'(lambda ()
                     (when closure-value
                       (prog1
                           closure-value
                         (setf closure-value nil)))))
               (constantly nil))))))

(def-cached-arg-parser char? (character)
  "Parser: accept token eql to argument"
  (sat (curry #'eql character)))

(def-cached-arg-parser string? (sequence)
  "Non-backtracking parser: accept a sequence of EQL elements."
  (let ((vector (coerce sequence 'vector)))
    (define-oneshot-result inp is-unread
      (iter (for c in-vector vector)
            (for inp-iter initially inp then (context-next inp-iter))
            (when (end-context-p inp-iter)
              (return nil))
            (for inp-data = (context-peek inp-iter))
            (unless (eql c inp-data)
              (return nil))
            (finally (return
                       (make-instance 'parser-possibility
                                      :tree (copy-seq sequence)
                                      :suffix inp-iter)))))))

(defun ensure-parser (parser)
  (typecase parser
    (function parser)
    (list (if (cdr parser)
              (string? parser)
              (char? (car parser))))
    (vector (if (length= 1 parser)
                (char? parser)
                (string? parser)))
    (t (char? parser))))

(defmacro with-parsers ((&rest parsers) &body body)
  `(let ,(iter (for p in parsers)
               (collect `(,p (ensure-parser ,p))))
     ,@body))