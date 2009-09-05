(in-package :parser-combinators)

(defmacro define-oneshot-result (inp is-unread &body body)
  `(function (lambda (,inp)
     (let ((,is-unread t))
       #'(lambda ()
           (when ,is-unread
             (setf ,is-unread nil)
             ,@body))))))

;;; emulating monads... did I even understand those?
;;; bind      :: Parser a -> (a -> Parser b) -> Parser b
;;;              (parser-tree1 function-from-tree1-to-parser-tree2)=>parser-tree2
;;; p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]
;;; (bind p f inp)=(concat list-comprehension)

(defun execute-bind (inp parser parser-generator) ;return continuation function
  (let ((p-parse-continuation (funcall parser inp))
        (q-parse-continuation nil))
    #'(lambda ()
        (let ((result nil))
          (iter (when q-parse-continuation (setf result (funcall q-parse-continuation)))
                (until (or result
                           (and (null p-parse-continuation)
                                (null q-parse-continuation))))
                (unless result
                  (setf q-parse-continuation
                        (let ((p-next-result
                               (funcall p-parse-continuation)))
                          (if p-next-result
                              (let ((v (tree-of p-next-result))
                                    (inp-prime (suffix-of p-next-result)))
                                (funcall (funcall parser-generator v) inp-prime))
                              (setf p-parse-continuation nil))))))
          result))))

(defmacro bind (parser parser-generator) ; results in parser-promise
  `(let ((parser ,parser)
         (parser-generator ,parser-generator))
     #'(lambda (inp)
         (execute-bind inp
                       parser
                       parser-generator))))

(defun execute-choice (inp parser1 parser2)
  (let ((continuation-1 (funcall parser1 inp))
        (continuation-2 nil))
    #'(lambda ()
        (cond (continuation-1
               (let ((result (funcall continuation-1)))
                 (unless result
                   (setf continuation-1 nil)
                   (setf continuation-2 (funcall parser2 inp))
                   (setf result (funcall continuation-2)))
                 result))
              (continuation-2
               (let ((result (funcall continuation-2)))
                 (unless result
                   (setf continuation-2 nil))
                 result))
              (t nil)))))

(defmacro choice (parser1 parser2)
  "Combinator: all alternatives from two parsers"
  `(let ((parser1 ,parser1)
         (parser2 ,parser2))
     #'(lambda (inp)
         (execute-choice inp parser1 parser2))))

(defmacro choice1 (parser1 parser2)
  "Combinator: one alternative from two parsers"
  `(let ((parser1 ,parser1)
         (parser2 ,parser2))
     #'(lambda (inp)
         (let ((parser1 parser1)
               (parser2 parser2)
               (is-unread t))
           #'(lambda ()
               (when is-unread
                 (setf is-unread nil)
                 (let ((result (funcall (execute-choice inp
                                                        parser1
                                                        parser2))))
                   (setf parser1 nil parser2 nil)
                   result)))))))

(defmacro choices (&rest parser-list)
  "Combinator: all alternatives from multiple parsers"
  (if (cdr parser-list)
      `(choice ,(car parser-list)
               (choices ,@(cdr parser-list)))
      (car parser-list)))

(defmacro choices1 (&rest parser-list)
  "Combinator: one alternative from multiple parsers"
  `(let ((parser-list (list ,@parser-list)))
     #'(lambda (inp)
         (let ((parser-list parser-list)
               (is-unread t))
           #'(lambda ()
               (when is-unread
                 (setf is-unread t)
                 (iter (for p in parser-list)
                       (for result = (funcall (funcall p inp)))
                       (finding result)
                       (finally (setf parser-list nil)))))))))
