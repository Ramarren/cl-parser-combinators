(in-package :parser-combinators)

(defmacro define-oneshot-result (inp is-unread &body body)
  `(function (lambda (,inp)
     (let ((,is-unread t))
       #'(lambda ()
           (when ,is-unread
             (setf ,is-unread nil)
             ,@body))))))

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

(def-cached-arg-parser char-equal? (character)
  "Parser: accept token char-equal to argument"
  (sat (curry #'char-equal character)))

(defgeneric string?-using-context (input vector test)
  (:documentation "Implementation of string? specialized on context type. Returns as multiple
  values result and new context or nil on failure.")
  (:method ((input context) vector test)
    (iter (for c in-vector vector)
          (for inp-iter initially input then (context-next inp-iter))
          (when (end-context-p inp-iter)
            (return nil))
          (for inp-data = (context-peek inp-iter))
          (unless (funcall test c inp-data)
            (return (values nil nil)))
          (finally (return 
                     (values (context-interval input inp-iter)
                             inp-iter)))))
  (:method ((input vector-context) vector test)
    (check-type vector vector)
    (let ((input-vector (storage-of input))
          (l (length vector))
          (p (position-of input)))
      (check-type input-vector vector)
      (if (> (+ l p)
             (length input-vector))
          (values nil nil)
          (let ((mismatch (mismatch input-vector vector
                                    :test test
                                    :start1 p
                                    :end1 (+ p l))))
            (if mismatch
                (values nil nil)
                (values (subseq input-vector p (+ p l))
                        (if (= (+ p l) (length input-vector))
                            (make-instance 'end-context
                                           :common (common-of input)
                                           :position (+ p l))
                            (make-instance 'vector-context
                                           :common (common-of input)
                                           :position (+ p l))))))))))

(def-cached-arg-parser string? (sequence &key (test #'eql) (result-type 'string))
  "Non-backtracking parser: accept a sequence of elements with equality tested by TEST."
  (let ((vector (coerce sequence 'vector)))
    (define-oneshot-result inp is-unread
      (multiple-value-bind (result new-input)
          (string?-using-context inp vector test)
        (when new-input
          (make-instance 'parser-possibility
                         :tree (coerce result result-type)
                         :suffix new-input))))))

(defun ensure-parser (parser)
  (typecase parser
    (function parser)
    (list (if (cdr parser)
              (string? parser)
              (char? (car parser))))
    (vector (if (length= 1 parser)
                (char? (aref parser 0))
                (string? parser)))
    (t (char? parser))))

(defmacro with-parsers ((&rest parsers) &body body)
  `(let ,(iter (for p in parsers)
               (collect `(,p (ensure-parser ,p))))
     ,@body))
