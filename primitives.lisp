(in-package :parser-combinators)

;;; primitive parsers
(declaim (inline result))
(defun result (v)
  "Primitive parser: return v, leaves input unmodified."
  #'(lambda (inp)
      (let ((closure-value (make-instance 'parser-possibility
                                          :tree v :suffix inp)))
        #'(lambda ()
            (when closure-value
              (prog1
                  closure-value
                (setf closure-value nil)))))))

(defun zero ()
  "Primitive parser: parsing failure"
  #'(lambda (inp)
      (declare (ignore inp))
      #'(lambda ()
          nil)))

(def-cached-parser item
  "Primitive parser: consume item from input and return it."
  #'(lambda (inp)
      (typecase inp
        (end-context (constantly nil))
        (context
           (let ((closure-value (make-instance 'parser-possibility
                                               :tree (context-peek inp) :suffix (context-next inp))))
             #'(lambda ()
                 (when closure-value
                   (prog1
                       closure-value
                     (setf closure-value nil)))))))))

(defun force? (parser)
  "Parser modifier: fully realize result from parser"
  #'(lambda (inp)
      (let ((continuation (funcall parser inp)))
        (let ((all-results (iter (for result = (funcall continuation))
                                 (while result)
                                 (collect result))))
          #'(lambda ()
              (when all-results
                (pop all-results)))))))

(defun cut? (parser)
  "Parser modifier: discard all results but the first"
  (choice1 parser (zero)))

(defmacro delayed? (parser)
  "Parser modifier macro: parser will be built when called. This is necessary for left-recursive parsers."
  `(let ((parser-cache nil))
     #'(lambda (inp)
         (unless parser-cache
           (setf parser-cache ,parser))
         (funcall parser-cache inp))))
