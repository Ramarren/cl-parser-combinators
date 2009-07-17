(in-package :parser-combinators)

;;; emulating monads... did I even understand those?
;;; bind      :: Parser a -> (a -> Parser b) -> Parser b
;;;              (parser-tree1 function-from-tree1-to-parser-tree2)=>parser-tree2
;;; p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]
;;; (bind p f inp)=(concat list-comprehension)

(defun execute-bind (inp parser parser-generator) ;return continuation function
  (let ((p-parse-result (funcall parser inp))
        (q-parse-result nil))
    #'(lambda ()
        (let ((result nil))
          (iter (when q-parse-result (setf result (next-result q-parse-result)))
                (until (or result
                           (and (null p-parse-result)
                                (null q-parse-result))))
                (unless result
                  (setf q-parse-result
                        (let ((p-next-result
                               (next-result p-parse-result)))
                          (if p-next-result
                              (let ((v (tree-of p-next-result))
                                    (inp-prime (suffix-of p-next-result)))
                                (funcall (funcall parser-generator v) inp-prime))
                              (setf p-parse-result nil))))))
          result))))

(defmacro bind (parser parser-generator) ; results in parser-promise
  `(let ((parser ,parser)
         (parser-generator ,parser-generator))
     #'(lambda (inp)
         (make-parse-result
          (execute-bind inp
                        parser
                        parser-generator)))))

(defun execute-choice (inp parser1 parser2)
  (let ((result1 (funcall parser1 inp))
        (result2 nil))
    #'(lambda ()
        (cond (result1
               (let ((result (next-result result1)))
                 (unless result
                   (setf result1 nil)
                   (setf result2 (funcall parser2 inp))
                   (setf result (next-result result2)))
                 result))
              (result2
               (let ((result (next-result result2)))
                 (unless result
                   (setf result2 nil)
                   result)))
              (t nil)))))

(defmacro choice (parser1 parser2)
  "Combinator: all alternatives from two parsers"
  `(let ((parser1 ,parser1)
         (parser2 ,parser2))
     #'(lambda (inp)
         (make-parse-result
          (execute-choice inp parser1 parser2)))))

(defmacro choice1 (parser1 parser2)
  "Combinator: one alternative from two parsers"
  `(let ((parser1 ,parser1)
         (parser2 ,parser2))
     #'(lambda (inp)
         (let ((parser1 parser1)
               (parser2 parser2)
               (is-unread t))
           (make-parse-result
            #'(lambda ()
                (when is-unread
                  (setf is-unread nil)
                  (let ((result (funcall (execute-choice inp
                                                         parser1
                                                         parser2))))
                    (setf parser1 nil parser2 nil)
                    result))))))))

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
           (make-parse-result
            #'(lambda ()
                (when is-unread
                  (setf is-unread t)
                  (iter (for p in parser-list)
                        (for result = (next-result (funcall p inp)))
                        (finding result)
                        (finally (setf parser-list nil))))))))))
