(in-package :parser-combinators)

;;; emulating monads... did I even understand those?
;;; bind      :: Parser a -> (a -> Parser b) -> Parser b
;;;              (parser-tree1 function-from-tree1-to-parser-tree2)=>parser-tree2
;;; p `bind` f = \inp -> concat [f v inp' | (v,inp') <- p inp]
;;; (bind p f inp)=(concat list-comprehension)

(defun execute-bind (inp parser parser-generator) ;return continuation function
  (with-parsers (parser)
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
                                  (funcall (ensure-parser (funcall parser-generator v)) inp-prime))
                                (setf p-parse-continuation nil))))))
            result)))))

(defun bind (parser parser-generator)
  #'(lambda (inp)
      (execute-bind inp
                    parser
                    parser-generator)))

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

(defun choice (parser1 parser2)
  "Combinator: all alternatives from two parsers"
  (let ((parser1 (ensure-parser parser1))
        (parser2 (ensure-parser parser2)))
     #'(lambda (inp)
         (execute-choice inp parser1 parser2))))

(defun choice1 (parser1 parser2)
  "Combinator: one alternative from two parsers"
  (let ((parser1 (ensure-parser parser1))
        (parser2 (ensure-parser parser2)))
    (define-oneshot-result inp is-unread
      (funcall (execute-choice inp
                               parser1
                               parser2)))))

(defun choices (&rest parser-list)
  "Combinator: all alternatives from multiple parsers"
  (if (cdr parser-list)
      (choice (car parser-list)
              (apply #'choices (cdr parser-list)))
      (car parser-list)))

(defun choices1 (&rest parser-list)
  "Combinator: one alternative from multiple parsers"
  (define-oneshot-result inp is-unread
    (iter (for p in parser-list)
          (for result = (funcall (funcall (ensure-parser p) inp)))
          (thereis result))))
