(in-package :parser-combinators)

;;; greedy version of repetition combinators

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
  (define-oneshot-result inp is-unread
    (iter (for count from 0)
          (for result next (funcall (funcall parser inp-prime)))
          (while (and result
                      (or (null max)
                          (< count max))))
          (for inp-prime initially inp then (suffix-of result))
          (collect result into results)
          (finally (return
                     (when (or (null min)
                               (>= count min))
                       (make-instance 'parser-possibility
                                      :tree (map result-type #'tree-of results)
                                      :suffix inp-prime)))))))

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
  (mdo (<- x parser-item)
       (<- xs (many* (mdo parser-separator
                          (<- y parser-item)
                          (result y))))
       (result (cons x xs))))

(defun sepby* (parser-item parser-separator)
  "Non-backtracking parser: accept as many as possible of parser-item separated by parser-separator."
  (choice1 (sepby1* parser-item parser-separator)
           (result nil)))

(defun chainl1* (p op)
  "Non-backtracking parser: accept as many as possible, but at least one of p, reduced by result of op with left associativity"
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
    (bind p #'rest-chain)))

(defun nat* ()
  "Non-backtracking parser: accept natural number, consuming as many digits as possible"
  (chainl1* (mdo (<- x (digit?))
                 (result (digit-char-p x)))
            (result
             #'(lambda (x y)
                 (+ (* 10 x) y)))))

(defun int* ()
  "Non-backtracking parser: accept integer, consuming as many digits as possible"
  (mdo (<- f (choice1 (mdo (char? #\-) (result #'-)) (result #'identity)))
       (<- n (nat*))
       (result (funcall f n))))

(defun chainr1* (p op)
  "Non-backtracking parser: accept as many as possible, but at least one of p, reduced by result of op with right associativity"
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
                               :tree init-x :suffix inp)))))))

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

(def-cached-arg-parser string? (sequence)
  "Non-backtracking parser: accept a sequence of EQL elements."
  (let ((vector (coerce sequence 'vector)))
    (define-oneshot-result inp is-unread
      (iter (for c in-vector vector)
            (for inp-iter initially inp then (context-next inp-iter))
            (when (typep inp-iter 'end-context)
              (return nil))
            (for inp-data = (context-peek inp-iter))
            (unless (eql c inp-data)
              (return nil))
            (finally (return
                       (make-instance 'parser-possibility
                                      :tree (copy-seq sequence)
                                      :suffix inp-iter)))))))

(def-cached-arg-parser times* (parser count)
    "Non-backtracking parser: accept exactly count expressions accepted by parser, without backtracking."
    (between* parser count count))

(defun find-after* (p q)
  "Non-backtracking parser: Find first q after some sequence of p."
  (define-oneshot-result inp is-unread
    (iter (for p-result next (funcall (funcall p inp-prime)))
          (for q-result next (funcall (funcall q inp-prime)))
          (while (and p-result (null q-result)))
          (for inp-prime initially inp then (suffix-of p-result))
          (finally (return
                     (when q-result
                       (make-instance 'parser-possibility
                                      :tree (tree-of q-result)
                                      :suffix (suffix-of q-result))))))))

(defun find-after-collect* (p q &optional (result-type 'list))
  "Non-backtracking parser: Find first q after some sequence of p. Return cons of list of p-results and q"
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
                                      :suffix (suffix-of q-result))))))))

(defun find* (q)
  "Non-backtracking parser: Find first q"
  (find-after* (item) q))

(defun expression* (term operators &optional (bracket-left nil) (bracket-right nil))
  "Non-backtracking parser: Reduce a sequence of terms with unary/binary operators with precedence.
 OPERATORS is a list of (op-parser :left/:right/:unary), where OP-PARSER is a parser consuming
 an operator and returning a reduction function. Highest precedence first."
  (let ((wrapped-term term))
    (labels ((term-wrapper (inp)
               (funcall wrapped-term inp)))
      (let ((expr-parser
             (iter (for (op assoc) in operators)
                   (for base initially #'term-wrapper
                        then (ecase assoc
                               (:left (chainl1* base op))
                               (:right (chainr1* base op))
                               (:unary (choice1
                                        (mdo (<- op-fun op)
                                             (<- subexpr base)
                                             (result (funcall op-fun subexpr)))
                                        base))))
                   (finally (return base)))))
        (when (and bracket-left bracket-right)
          (setf wrapped-term (choice1 (bracket? bracket-left expr-parser bracket-right)
                                      term)))
        expr-parser))))
