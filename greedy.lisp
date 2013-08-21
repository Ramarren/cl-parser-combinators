(in-package :parser-combinators)

;;; greedy version of repetition combinators

(defun seq-list* (&rest parsers)
  "Non-backtracking parser: Return a list of result of PARSERS."
  (assert parsers)
  (let ((parsers (map 'vector #'ensure-parser parsers)))
    (define-oneshot-result inp is-unread
      (iter (for parser in-vector parsers)
            (for inp-prime initially inp then (suffix-of result))
            (for result = (funcall (funcall parser inp-prime)))
            (while result)
            (collect result into results)
            (finally (return
                       (when result
                         (make-instance 'parser-possibility
                                        :tree (mapcar #'tree-of results)
                                        :suffix (suffix-of result)))))))))

(defmacro named-seq* (&rest parser-descriptions)
  "Non-backtracking parser: This is similar to MDO, except that constructed parsers cannot depend on
the results of previous ones and the final form is not used as a parser, but is automatically used
to construct the result. All names bound using the (<- name parser) construct are only available in
that final form.

This parser generator is useful when full generality of MDO is not necessary, as it is implemented
non-recursively and has better memory performance."
  `(%named-seq? seq-list* ,@parser-descriptions))

(defmacro mdo* (&body spec)
  "Like NAMED-SEQ*, but with MDO syntax: the last element must be a parser."
  (with-gensyms (ret)
    `(named-seq*
      ,@(butlast spec)
      (<- ,ret ,(lastcar spec))
      ,ret)))

(defun between* (parser min max &optional (result-type 'list))
  "Non-backtracking parser: find the first, longest chain of expression accepted by parser of length between min and max"
  (assert (or (null min)
              (null max)
              (>= max min)))
  ;; min=zero or nil means accept zero width results
  (assert (or (null min)
              (zerop min)
              (plusp min)))
  ;; can't have 0-0 parser
  (assert (or (null max)
              (plusp max)))
  (with-parsers (parser)
    (define-oneshot-result inp is-unread
      (iter (for count from 0)
            (for result next (funcall (funcall parser inp-prime)))
            (while (and result
                        (or (null max)
                            (< count max))))
            (for inp-prime initially inp then
                 (if (eql inp-prime (suffix-of result))
                     (error "Subparser in repetition parser didn't advance the input.")
                     (suffix-of result)))
            (collect result into results)
            (finally (return
                       (when (or (null min)
                                 (>= count min))
                         (make-instance 'parser-possibility
                                        :tree (map result-type #'tree-of results)
                                        :suffix inp-prime))))))))

(defun many* (parser)
  "Non-backtracking parser: collect as many of first result of parser as possible"
  (between* parser nil nil))

(defun many1* (parser)
  "Non-backtracking parser: accept as many as possible, and at least one, of parser"
  (between* parser 1 nil))

(defun atleast* (parser count)
  "Non-backtracking parser: accept as many as possible and at least count of parser"
  (between* parser count nil))

(defun atmost* (parser count)
  "Non-backtracking parser: accept as many as possible but at most count of parser"
  (between* parser nil count))

(defun sepby1* (parser-item parser-separator)
  "Non-backtracking parser: accept as many as possible of parser-item separated by parser-separator, but at least one."
  (named-seq* (<- x parser-item)
              (<- xs (many* (named-seq* parser-separator
                                        (<- y parser-item)
                                        y)))
              (cons x xs)))

(defun sepby* (parser-item parser-separator)
  "Non-backtracking parser: accept as many as possible of parser-item separated by parser-separator."
  (choice1 (sepby1* parser-item parser-separator)
           (result nil)))

(defun chainl1* (p op)
  "Non-backtracking parser: accept as many as possible, but at least one of p, reduced by result of op with left associativity"
  (with-parsers (p op)
    (labels ((rest-chain (init-x)
               (define-oneshot-result inp is-unread
                 (let ((final-result (iter (for f-result next (funcall (funcall op p-inp)))
                                           (while f-result)
                                           (for f-inp next (suffix-of f-result))
                                           (for p-result next (funcall (funcall p f-inp)))
                                           (while p-result)
                                           (for p-inp initially inp then (suffix-of p-result))
                                           (for f = (tree-of f-result))
                                           (for x initially init-x then tree)
                                           (for y = (tree-of p-result))
                                           (for tree next (funcall f x y))
                                           (finally (return (list tree p-inp))))))
                   (if (car final-result)
                       (make-instance 'parser-possibility
                                      :tree (car final-result)
                                      :suffix (cadr final-result))
                       (make-instance 'parser-possibility
                                      :tree init-x :suffix inp))))))
      (bind p #'rest-chain))))

(def-cached-arg-parser whitespace* (&key (result-type nil) (accept-empty nil))
  "Non-backtracking parser: accept a sequence of whitespace characters."
  (gather-if* (rcurry #'member '(#\Space #\Newline #\	))
              :result-type result-type
              :accept-empty accept-empty))

(def-cached-parser word*
  "Parser: accept a string of alphanumeric characters"
  (gather-if* #'alphanumericp :result-type 'string))

(def-cached-parser pure-word*
  "Parser: accept a string of alphabetic characters"
  (gather-if* #'alpha-char-p :result-type 'string))

(defun nat* (&optional (radix 10))
  "Non-backtracking parser: accept natural number, consuming as many digits as possible"
  (named-seq* (<- number (gather-if* (rcurry #'digit-char-p radix) :result-type 'string))
   (parse-integer number :radix radix)))

(defun int* (&optional (radix 10))
  "Non-backtracking parser: accept integer, consuming as many digits as possible"
  (named-seq* (<- sign (choices1 #\+ #\- (result #\+)))
              (<- n (nat* radix))
              (* (if (eql sign #\+) 1 -1) n)))

(defun chainr1* (p op)
  "Non-backtracking parser: accept as many as possible, but at least one of p, reduced by result of op with right associativity"
  (with-parsers (p op)
    (bind p
      #'(lambda (init-x)
          (define-oneshot-result inp is-unread
            (let ((final-result
                   (iter (for f-result next (funcall (funcall op p-inp)))
                         (while f-result)
                         (for f-inp next (suffix-of f-result))
                         (for p-result next (funcall (funcall p f-inp)))
                         (while p-result)
                         (for p-inp initially inp then (suffix-of p-result))
                         (for f = (tree-of f-result))
                         (for y = (tree-of p-result))
                         (collect f into function-list)
                         (collect y into y-list)
                         (finally (let ((rev-y-list (nreverse (cons init-x y-list))))
                                    (return (list (iter (for x in (cdr rev-y-list))
                                                        (for f in function-list)
                                                        (for tree next (if (first-iteration-p)
                                                                           (funcall f x (car rev-y-list))
                                                                           (funcall f x tree)))
                                                        (finally (return tree)))
                                                  p-inp)))))))
              (if (car final-result)
                  (make-instance 'parser-possibility
                                 :tree (car final-result)
                                 :suffix (cadr final-result))
                  (make-instance 'parser-possibility
                                 :tree init-x :suffix inp))))))))

(defun chainl* (p op v)
  "Non-backtracking parser: like chainl1*, but will return v if no p can be parsed"
  (choice1
   (chainl1* p op)
   (result v)))

(defun chainr* (p op v)
  "Non-backtracking parser: like chainr1*, but will return v if no p can be parsed"
  (choice1
   (chainr1* p op)
   (result v)))

(def-cached-arg-parser times* (parser count)
    "Non-backtracking parser: accept exactly count expressions accepted by parser, without backtracking."
    (between* parser count count))

(defun find-after* (p q)
  "Non-backtracking parser: Find first q after some sequence of p."
  (with-parsers (p q)
    (define-oneshot-result inp is-unread
      (iter (for p-result next (funcall (funcall p inp-prime)))
            (for q-result next (funcall (funcall q inp-prime)))
            (while (and p-result (null q-result)))
            (for inp-prime initially inp then (suffix-of p-result))
            (finally (return
                       (when q-result
                         (make-instance 'parser-possibility
                                        :tree (tree-of q-result)
                                        :suffix (suffix-of q-result)))))))))

(defgeneric gather-if-not*-using-context (input predicate accept-end accept-empty)
  (:documentation "Parser gather-if-not* specialized on context type")
  (:method ((input end-context) predicate accept-end accept-empty)
    (if (and accept-end accept-empty)
        (values nil input)
        (values nil nil)))
  (:method ((input context) predicate accept-end accept-empty)
    (iter (until (or (end-context-p inp-prime)
                     (funcall predicate (context-peek inp-prime))))
          (for inp-prime initially input then (context-next inp-prime))
          (collect (context-peek inp-prime) into results)
          (finally (return
                     (when (and results
                                (or (and accept-end (end-context-p inp-prime))
                                    (funcall predicate (context-peek inp-prime))))
                       (values results inp-prime))))))
  (:method ((input vector-context) predicate accept-end accept-empty)
    (let ((input-vector (storage-of input)))
      (check-type input-vector vector)
      (let ((end-position (position-if predicate input-vector :start (position-of input))))
        (cond ((and accept-end (null end-position))
               (values (subseq input-vector (position-of input))
                       (make-context-at-position input (length input-vector))))
              ((and end-position (or accept-empty (> end-position (position-of input))))
               (values (subseq input-vector (position-of input) end-position)
                       (make-context-at-position input end-position)))
              (t (values nil nil)))))))

(defun gather-if-not* (predicate &key (result-type 'list) (accept-end nil) (accept-empty nil))
  "Non-backtracking parser: Find a sequence of tokens terminated by one for which predicate returns true, which is not consumed."
  (define-oneshot-result inp is-unread
    (multiple-value-bind (result new-input) (gather-if-not*-using-context inp predicate accept-end accept-empty)
      (when new-input
        (make-instance 'parser-possibility
                       :tree (when result-type (coerce result result-type))
                       :suffix new-input)))))

(defun gather-if* (predicate &key (result-type 'list) (accept-end t) (accept-empty nil))
  "Non-backtracking parser: Find a sequence of tokens for which predicate returns true."
  (gather-if-not* (complement predicate)
                  :result-type result-type
                  :accept-end accept-end
                  :accept-empty accept-empty))

(defun gather-before-token* (token &key (result-type 'list) (test #'eql) (accept-end nil) (accept-empty nil))
  "Non-backtracking parser: Find a sequence of tokens terminated by single token, which is not consumed."
  (gather-if-not* #'(lambda (input-token)
                      (funcall test input-token token))
                  :result-type result-type
                  :accept-end accept-end
                  :accept-empty accept-empty))

(defun find-before-token* (p token &key (result-type 'list) (test #'eql))
  "Non-backtracking parser: Find a sequence of p terminated by single token q, which is not consumed."
  (with-parsers (p)
    (define-oneshot-result inp is-unread
      (iter (for p-result next (funcall (funcall p inp-prime)))
            (while (and p-result (not (funcall test (context-peek inp-prime) token))))
            (for inp-prime initially inp then (suffix-of p-result))
            (collect (tree-of p-result) into p-results)
            (finally (return
                       (when (funcall test (context-peek inp-prime) token)
                         (make-instance 'parser-possibility
                                        :tree (coerce p-results result-type)
                                        :suffix inp-prime))))))))


(defun find-before* (p q &optional (result-type 'list))
  "Non-backtracking parser: Find a sequence of p terminated by q, doesn't consume q."
  (with-parsers (p q)
    (define-oneshot-result inp is-unread
      (iter (for p-result next (funcall (funcall p inp-prime)))
            (for q-result next (funcall (funcall q inp-prime)))
            (while (and p-result (null q-result)))
            (for inp-prime initially inp then (suffix-of p-result))
            (collect (tree-of p-result) into p-results)
            (finally (return
                       (when q-result
                         (make-instance 'parser-possibility
                                        :tree (coerce p-results result-type)
                                        :suffix inp-prime))))))))

(defun find-after-collect* (p q &optional (result-type 'list))
  "Non-backtracking parser: Find first q after some sequence of p. Return cons of list of p-results and q"
  (with-parsers (p q)
    (define-oneshot-result inp is-unread
      (iter (for p-result next (funcall (funcall p inp-prime)))
            (for q-result next (funcall (funcall q inp-prime)))
            (while (and p-result (null q-result)))
            (collect p-result into p-results)
            (for inp-prime initially inp then (suffix-of p-result))
            (finally (return
                       (when q-result
                         (make-instance 'parser-possibility
                                        :tree (cons (map result-type #'tree-of p-results)
                                                    (tree-of q-result))
                                        :suffix (suffix-of q-result)))))))))

(defun before* (p q)
  "Non-backtracking parser: Find a p before q, doesn't consume q."
  (with-parsers (p q)
    (define-oneshot-result inp is-unread
      (let ((p-result (funcall (funcall p inp))))
        (when p-result
          (let* ((p-suffix (suffix-of p-result))
                 (q-result (funcall (funcall q p-suffix))))
            (when (and p-result q-result)
              (make-instance 'parser-possibility :tree (tree-of p-result) :suffix p-suffix))))))))

(defun find* (q)
  "Non-backtracking parser: Find first q"
  (find-after* (item) q))

(defun opt* (p)
  "Non-backtracking parser: result of p or nil"
  (choice1 p (result nil)))

(defun expression* (term operators &optional (bracket-left nil) (bracket-right nil))
  "Non-backtracking parser: Reduce a sequence of terms with unary/binary operators with precedence.
 OPERATORS is a list of (op-parser :left/:right/:unary), where OP-PARSER is a parser consuming
 an operator and returning a reduction function. Highest precedence first."
  (with-parsers (term bracket-left bracket-right)
    (named? expr-parser
      (iter (for (op assoc) in operators)
            (for base initially (choice1 (bracket? bracket-left expr-parser bracket-right)
                                         term)
                 then (ecase assoc
                        (:left (chainl1* base op))
                        (:right (chainr1* base op))
                        (:unary (choice1
                                 (named-seq* (<- op-fun op)
                                             (<- subexpr base)
                                             (funcall op-fun subexpr))
                                base))))
            (finally (return base))))))
