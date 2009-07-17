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

(defmacro cached-arguments? (parser label &rest arguments)
  "Parser modifier macro: cache parser as label with argument list equal under equal in global cache."
  (with-unique-names (inp cache subcache args)
    `(let ((,args ,(cons 'list arguments)))
      #'(lambda (,inp)
          (unless (gethash ',label *parser-cache*)
            (setf (gethash ',label *parser-cache*) (make-hash-table :test 'equal)))
          (let ((,cache (gethash ',label *parser-cache*)))
            (if-let ((,subcache (gethash ,args ,cache)))
              (funcall ,subcache ,inp)
              (funcall (setf (gethash ,args ,cache) ,parser) ,inp)))))))

(defmacro def-cached-parser (name &body body)
  "Define cached parser of no arguments."
  (multiple-value-bind (forms declarations docstring) (parse-body body :documentation t)
   `(defun ,name ()
      ,docstring
      ,@declarations
      (cached? ,@forms ,(gensym)))))

(defmacro def-cached-arg-parser (name arguments &body body)
  "Define cached parser with arguments."
  (multiple-value-bind (forms declarations docstring) (parse-body body :documentation t)
   `(defun ,name ,arguments
      ,docstring
      ,@declarations
      (cached-arguments? ,@forms ,(gensym) ,@arguments))))

;;; primitive parsers
(declaim (inline result))
(defun result (v)
  "Primitive parser: return v, leaves input unmodified."
  #'(lambda (inp)
      (let ((closure-value (make-instance 'parser-possibility
                                          :tree v :suffix inp)))
        (make-parse-result
         #'(lambda ()
             (when closure-value
               (prog1
                   closure-value
                 (setf closure-value nil))))))))

(defun zero ()
  "Primitive parser: parsing failure"
  (constantly (make-instance 'parse-result)))

(def-cached-parser item
  "Primitive parser: consume item from input and return it."
  #'(lambda (inp)
      (typecase inp
        (end-context (make-instance 'parse-result))
        (context
           (let ((closure-value (make-instance 'parser-possibility
                                               :tree (context-peek inp) :suffix (context-next inp))))
             (make-parse-result
              #'(lambda ()
                  (when closure-value
                    (prog1
                        closure-value
                      (setf closure-value nil))))))))))

(declaim (inline sat))
(def-cached-arg-parser sat (predicate)
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
          (make-parse-result
           #'(lambda ()
               (pop all-results)))))))

(defmacro delayed? (parser)
  "Parser modifier macro: parser will be built when called. This is necessary for left-recursive parsers."
  `(let ((parser-cache nil))
     #'(lambda (inp)
         (unless parser-cache
           (setf parser-cache ,parser))
         (funcall parser-cache inp))))
