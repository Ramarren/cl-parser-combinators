(in-package :parser-combinators)

(defun tag? (parser format-control &rest format-arguments)
  (let ((tag (apply #'format nil format-control format-arguments)))
    (with-parsers (parser)
      #'(lambda (inp)
          (let ((*tag-stack* (cons tag *tag-stack*)))
            (funcall parser inp))))))

(def-cached-parser context?
  "Parser: return current context without consuming any input"
  (define-oneshot-result inp is-unread
    (make-instance 'parser-possibility :tree inp :suffix inp)))

(def-cached-parser end?
  "Parser: matches end of input, returns t"
  (define-oneshot-result inp is-unread
    (when (end-context-p inp)
      (make-instance 'parser-possibility :tree t :suffix inp))))

(def-cached-parser digit?
  "Parser: accept digit character"
  (sat #'digit-char-p))

(def-cached-parser lower?
  "Parser: accept lowercase character"
  (sat #'lower-case-p))

(def-cached-parser upper?
  "Parser: accept uppercase character"
  (sat #'upper-case-p))

(def-cached-parser letter?
  "Parser: accept alphabetic character"
  (sat #'alpha-char-p))

(def-cached-parser alphanum?
  "Parser: accept alphanumeric character"
  (sat #'alphanumericp))

;;; implement repetition parsers in terms of (between? ...)

(defun between? (parser min max &optional (result-type 'list))
  "Parser: accept between min and max expressions accepted by parser"
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
  ;; gather results depth-first, longest first, ie. gather shorter on returning
  (with-parsers (parser)
    #'(lambda (inp)
        (let ((continuation-stack nil)
              (result-stack nil)
              (count 1)
              (zero-width (or (null min)
                              (zerop min)))
              (state :next-result))
          (push (funcall parser inp) continuation-stack)
          #'(lambda ()
              (setf state :next-result)
              (iter
                ;; (print state)
                ;; (print result-stack)
                ;; (print zero-width)
                ;; (print count)
                (while (or continuation-stack
                           zero-width))
                (ecase state
                  (:next-result
                     (let ((next-result (funcall (car continuation-stack))))
                       (cond ((null next-result)
                              (pop continuation-stack)
                              (decf count)
                              (setf state :check-count))
                             ((and max (= count max))
                              (push next-result result-stack)
                              (setf state :return))
                             (t
                              (incf count)
                              (when (and result-stack
                                         (eq (suffix-of (car result-stack))
                                             (suffix-of next-result)))
                                (error "Subparser in repetition parser didn't advance the input."))
                              (push next-result result-stack)
                              (push (funcall parser (suffix-of next-result)) continuation-stack)))))
                  (:check-count
                     (cond ((or (null continuation-stack)
                                (and (or (null min)
                                         (>= count min))
                                     (or (null max)
                                         (<= count max))))
                            (setf state :return))
                           (t (pop result-stack)
                            (setf state :next-result))))
                  (:return
                    (return
                      (cond (result-stack
                             (let ((result
                                    (make-instance 'parser-possibility
                                                   :tree (map result-type #'tree-of (reverse result-stack))
                                                   :suffix (suffix-of (car result-stack)))))
                               (pop result-stack)
                               result))
                            (zero-width
                             (setf zero-width nil)
                             (make-instance 'parser-possibility
                                            :tree nil
                                            :suffix inp))))))))))))

(def-cached-parser word?
  "Parser: accept a string of alphabetic characters"
  (between? (letter?) 1 nil 'string))

(defun many? (parser)
  "Parser: accept zero or more repetitions of expression accepted by parser"
  (between? parser nil nil))

(defun many1? (parser)
  "Parser: accept one or more of expression accepted by parser"
  (between? parser 1 nil))

(defun times? (parser count)
  "Parser: accept exactly count expressions accepted by parser"
  (between? parser count count))

(defun atleast? (parser count)
  "Parser: accept at least count expressions accepted by parser"
  (between? parser count nil))

(defun atmost? (parser count)
  "Parser: accept at most count expressions accepted by parser"
  (between? parser nil count))

(defun int? ()
  "Parser: accept an integer"
  (mdo (<- f (choice (mdo (char? #\-) (result #'-)) (result #'identity)))
       (<- n (nat?))
       (result (funcall f n))))

(defun sepby1? (parser-item parser-separator)
  "Parser: accept at least one of parser-item separated by parser-separator"
  (with-parsers (parser-item parser-separator)
    (mdo (<- x parser-item)
         (<- xs (many? (mdo parser-separator (<- y parser-item) (result y))))
         (result (cons x xs)))))

(defun bracket? (parser-open parser-center parser-close)
  "Parser: accept parser-center bracketed by parser-open and parser-close"
  (with-parsers (parser-open parser-center parser-close)
    (mdo parser-open (<- xs parser-center) parser-close (result xs))))

(defun sepby? (parser-item parser-separator)
  "Parser: accept zero or more of parser-item separated by parser-separator"
  (with-parsers (parser-item parser-separator)
    (choice (sepby1? parser-item parser-separator) (result nil))))

;; since all intermediate results have to be kept anyway for backtracking, they might be just as
;; well be kept not on the stack, so chainl/r1? can be implemented in terms of between? as well

(defun sepby1-cons? (p op)
  "Parser: as sepby1, but returns a list of a result of p and pairs (op p). Mainly a component parser for chains"
  (with-parsers (p op)
    (let ((between-parser (between? (mdo (<- op-result op)
                                         (<- p-result p)
                                         (result (cons op-result p-result)))
                                    1 nil)))
      #'(lambda (inp)
          (let ((front-continuation (funcall p inp))
                (between-continuation nil)
                (front nil)
                (chain nil)
                (state :next-result))
            #'(lambda ()
                (setf state :next-result)
                (iter
                  (ecase state
                    (:next-result
                       (cond (between-continuation
                              (if-let (next-chain (funcall between-continuation))
                                (setf chain next-chain
                                      state :return)
                                (setf between-continuation nil
                                      chain nil
                                      state :return)))
                             (front-continuation
                              (if-let (next-front (funcall front-continuation))
                                (setf front next-front
                                      between-continuation (funcall between-parser (suffix-of next-front)))
                                (setf front-continuation nil)))
                             (t (setf state :return))))
                    (:return
                      (return (cond
                                (chain (make-instance 'parser-possibility
                                                      :suffix (suffix-of chain)
                                                      :tree (cons (tree-of front) (tree-of chain))))
                                (front (prog1 (make-instance 'parser-possibility
                                                             :tree (list (tree-of front))
                                                             :suffix (suffix-of front))
                                         (setf front nil))))))))))))))

(defun chainl1? (p op)
  "Parser: accept one or more p reduced by result of op with left associativity"
  (with-parsers (p op)
    (let ((subparser (sepby1-cons? p op)))
      (mdo (<- chain subparser)
           (result
            (destructuring-bind (front . chain) chain
              (iter (for left initially front
                         then (funcall op
                                       left
                                       right))
                    (for (op . right) in chain)
                    (finally (return left)))))))))

(defun nat? ()
  "Parser: accept natural numbers"
  (chainl1? (mdo (<- x (digit?))
                 (result (digit-char-p x)))
            (result
             #'(lambda (x y)
                 (+ (* 10 x) y)))))

(defun chainr1? (p op)
  "Parser: accept one or more p reduced by result of op with right associativity"
  (with-parsers (p op)
    (let ((subparser (sepby1-cons? p op)))
      (mdo (<- chain subparser)
           (result
            (destructuring-bind (front . chain) chain
              (iter (with chain = (reverse chain))
                    (with current-op = (car (car chain)))
                    (with current-right = (cdr (car chain)))
                    (for (op . right) in (cdr chain))
                    (setf current-right (funcall current-op right current-right)
                          current-op op)
                    (finally (return (funcall op front current-right))))))))))

(defun chainl? (p op v)
  "Parser: like chainl1?, but will return v if no p can be parsed"
  (with-parsers (p op)
    (choice
     (chainl1? p op)
     (result v))))

(defun chainr? (p op v)
  "Parser: like chainr1?, but will return v if no p can be parsed"
  (with-parsers (p op)
    (choice
     (chainr1? p op)
     (result v))))

(defclass result-node (parser-possibility)
  ((emit :initarg :emit :initform t :accessor emit-of)
   (up :initarg :up :initform nil :accessor up-of)
   (count :initarg :count :initform 0 :accessor count-of)
   (suffix-continuation :initarg :suffix-continuation :accessor suffix-continuation-of)))

(defun gather-nodes (node)
  (let ((nodes))
    (iter (for current-node initially node then (up-of current-node))
          (while current-node)
          (when (emit-of current-node)
           (push current-node nodes))
          (finally (return nodes)))))

(defun breadth? (parser min max &optional (result-type 'list))
  "Parser: like between? but breadth first (shortest matches first)"
  (with-parsers (parser)
    #'(lambda (inp)
        (let ((queue (make-queue (list
                                  (make-instance 'result-node
                                                 :suffix inp
                                                 :suffix-continuation (funcall parser inp)
                                                 :tree nil
                                                 :emit nil
                                                 :up nil))))
              (node nil))
          #'(lambda ()
              (iter
                (until (empty-p queue))
                (setf node (pop-front queue))
                (for count = (count-of node))
                (iter (for result next (funcall (suffix-continuation-of node)))
                      (while result)
                      (for suffix = (suffix-of result))
                      (unless (and max
                                   (= count max))
                        (push-back queue (make-instance 'result-node
                                                        :suffix suffix
                                                        :suffix-continuation (funcall parser suffix)
                                                        :up node
                                                        :count (1+ count)
                                                        :tree (tree-of result)))))
                (when (and (emit-of node)
                           (or (null min)
                               (>= count min)))
                  (return (make-instance 'parser-possibility
                                         :tree (map result-type #'tree-of (gather-nodes node))
                                         :suffix (suffix-of node))))))))))

(defun find-after? (p q)
  "Parser: Find first q after some sequence of p."
  (with-parsers (p q)
    (mdo (breadth? p nil nil nil)
         q)))

(defun find-before? (p q &optional (result-type 'list))
  "Parser: Find a sequence of p terminated by q, doesn't consume q."
  (with-parsers (p q)
    #'(lambda (inp)
        (let ((p-parse-continuation (funcall (breadth? p nil nil result-type) inp)))
          #'(lambda ()
              (let ((result nil))
                (iter (until (or result
                                 (null p-parse-continuation)))
                      (for p-result = (funcall p-parse-continuation))
                      (if p-result
                          (when (funcall (funcall q (suffix-of p-result)))
                            (setf result p-result))
                          (setf p-parse-continuation nil)))
                result))))))

(defun find-after-collect? (p q &optional (result-type 'list))
  "Parser: Find first q after some sequence of p. Return cons of list of p-results and q"
  (with-parsers (p q)
    (mdo (<- prefix (breadth? p nil nil result-type))
         (<- q-result q)
         (result (cons prefix q-result)))))

(defun find? (q)
  "Parser: Find first q"
  (with-parsers (q)
    (find-after? (item) q)))

(defun expression? (term operators &optional (bracket-left nil) (bracket-right nil))
  "Parser: Reduce a sequence of terms with unary/binary operators with precedence.
 OPERATORS is a list of (op-parser :left/:right/:unary), where OP-PARSER is a parser consuming
 an operator and returning a reduction function. Highest precedence first."
  (with-parsers (term bracket-left bracket-right)
    (let ((wrapped-term term))
      (labels ((term-wrapper (inp)
                 (funcall wrapped-term inp)))
        (let ((expr-parser
               (iter (for (op assoc) in operators)
                     (for base initially #'term-wrapper
                          then (ecase assoc
                                 (:left (chainl1? base op))
                                 (:right (chainr1? base op))
                                 (:unary (choice
                                          (mdo (<- op-fun op)
                                               (<- subexpr base)
                                               (result (funcall op-fun subexpr)))
                                          base))))
                     (finally (return base)))))
          (when (and bracket-left bracket-right)
            (setf wrapped-term (choice (bracket? bracket-left expr-parser bracket-right)
                                       term)))
          expr-parser)))))
