(in-package :parser-combinators)

(defvar *parser-cache* (make-hash-table))

(defun drop-parser-cache ()
  (clrhash *parser-cache*))

;; parser caching

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

(defmacro cached-arguments? (parser label &rest arguments)
  "Parser modifier macro: cache parser as label with argument list equal under equal in global cache."
  (with-unique-names (inp cache subcache args)
    (let ((filtered-arguments (mapcar #'ensure-car
                                      (remove-if (rcurry #'member '(&optional &key &rest)) arguments))))
      `(let ((,args ,(cons 'list filtered-arguments)))
         #'(lambda (,inp)
             (unless (gethash ',label *parser-cache*)
               (setf (gethash ',label *parser-cache*) (make-hash-table :test 'equal)))
             (let ((,cache (gethash ',label *parser-cache*)))
               (if-let ((,subcache (gethash ,args ,cache)))
                 (funcall ,subcache ,inp)
                 (funcall (setf (gethash ,args ,cache) ,parser) ,inp))))))))

(defmacro def-cached-arg-parser (name arguments &body body)
  "Define cached parser with arguments."
  (multiple-value-bind (forms declarations docstring) (parse-body body :documentation t)
   `(defun ,name ,arguments
      ,docstring
      ,@declarations
      (cached-arguments? ,@forms ,(gensym) ,@arguments))))
