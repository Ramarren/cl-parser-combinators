(in-package :parser-combinators)

;;; primitive parsers
(declaim (inline result))
(defun result (v)
  "Primitive parser: return v, leaves input unmodified."
  #'(lambda (inp)
      (let ((closure-value (make-instance 'parser-possibility
                                          :tree v :suffix inp)))
        (make-instance 'parse-result
                       :continuation #'(lambda ()
                                         (when closure-value
                                           (prog1
                                               closure-value
                                             (setf closure-value nil))))))))

(defun zero ()
  "Primitive parser: parsing failure"
  (constantly (make-instance 'parse-result)))

(defun item ()
    "Primitive parser: consume item from input and return it."
  #'(lambda (inp)
      (typecase inp
        (end-context (make-instance 'parse-result))
        (context
           (let ((closure-value (make-instance 'parser-possibility
                                               :tree (context-peek inp) :suffix (context-next inp))))
             (make-instance 'parse-result
                            :continuation #'(lambda ()
                                              (when closure-value
                                                (prog1
                                                    closure-value
                                                  (setf closure-value nil))))))))))

(declaim (inline sat))
(defun sat (predicate)
  "Parser: return a token satisfying a predicate."
  (bind (item) #'(lambda (x)
                   (if (funcall predicate x)
                       (result x)
                       (zero)))))

(defun force? (parser)
  "Parser modifier: fully realize result from parser"
  #'(lambda (inp)
      (let ((result (funcall parser inp)))
        (let ((all-results (gather-results result)))
          (make-instance 'parse-result
                         :continuation #'(lambda ()
                                           (pop all-results)))))))

(defmacro delayed? (parser)
  "Parser modifier macro: parser will be built when called. This is necessary for left-recursive parsers."
  `(let ((parser-cache nil))
     #'(lambda (inp)
         (unless parser-cache
           (setf parser-cache ,parser))
         (funcall parser-cache inp))))
