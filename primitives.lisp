(in-package :parser-combinators)

;; parser caching

(defvar *parser-cache* (make-hash-table))

(defmacro cached? (parser label)
  "Parser modifier macro: cache parser as label in global cache."
  (with-unique-names (inp cache)
    `#'(lambda (,inp)
         (if-let ((,cache (gethash ',label *parser-cache*)))
           (funcall ,cache ,inp)
           (funcall (setf (gethash ',label *parser-cache*) ,parser) ,inp)))))

(defmacro def-cached-parser (name &body body)
  "Define cached parser of no arguments."
  (multiple-value-bind (forms declarations docstring) (parse-body body :documentation t)
   `(defun ,name ()
      ,docstring
      ,@declarations
      (cached? ,@forms ,(gensym)))))

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
  (constantly nil))

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

(defmacro delayed? (parser)
  "Parser modifier macro: parser will be built when called. This is necessary for left-recursive parsers."
  `(let ((parser-cache nil))
     #'(lambda (inp)
         (unless parser-cache
           (setf parser-cache ,parser))
         (funcall parser-cache inp))))
