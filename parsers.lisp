(in-package :parser-combinators)

(defun seq-list? (&rest parsers)
  "Parser: Return a list of results of PARSERS."
  (assert parsers)
  (let ((parsers (map 'vector #'ensure-parser parsers)))
    #'(lambda (inp)
        (let ((continuation-stack (list (funcall (aref parsers 0) inp)))
              (result-stack nil)
              (continuation-count 1)
              (result-count 0)
              (l (length parsers)))
          #'(lambda ()
              (iter (while continuation-stack)
                    (until (= result-count l))
                    (let ((next-result (funcall (car continuation-stack))))
                      (cond ((null next-result)
                             (pop continuation-stack)
                             (decf continuation-count)
                             (pop result-stack)
                             (decf result-count))
                            ((= continuation-count l)
                             (incf result-count)
                             (push next-result result-stack))
                            (t
                             (incf result-count)
                             (push next-result result-stack)
                             (incf continuation-count)
                             (push (funcall (aref parsers result-count)
                                            (suffix-of next-result))
                                   continuation-stack))))
                    (finally
                     (return
                       (when result-stack
                         (let ((result
                                (make-instance 'parser-possibility
                                               :tree (mapcar #'tree-of (reverse result-stack))
                                               :suffix (suffix-of (car result-stack)))))
                           (decf result-count)
                           (pop result-stack)
                           result))))))))))

(defmacro %named-seq? (sequence-parser &rest parser-descriptions)
  (assert (> (length parser-descriptions) 1))
  (let ((name-vector (make-array (1- (length parser-descriptions)) :initial-element nil))
        (parsers nil)
        (result-form nil)
        (gensym-list nil))
    (iter (for description in parser-descriptions)
          (for i from 0)
          (cond ((= i (length name-vector))
                 (setf result-form description))
                ((and (listp description)
                      (eql (car description) '<-))
                 (setf (aref name-vector i) (second description))
                 (push (third description) parsers))
                (t
                 (push description parsers)
                 (let ((gensym (gensym)))
                   (push gensym gensym-list)
                   (setf (aref name-vector i) gensym)))))
    (with-unique-names (inp continuation seq-parser result)
      `(let ((,seq-parser (,sequence-parser ,@(nreverse parsers))))
         #'(lambda (,inp)
             (let ((,continuation (funcall ,seq-parser ,inp)))
               #'(lambda ()
                   (when ,continuation
                     (let ((,result (funcall ,continuation)))
                       (if ,result
                           (destructuring-bind ,(map 'list #'identity name-vector)
                               (tree-of ,result)
                             ,@(when gensym-list
                                 (list `(declare (ignore ,@gensym-list))))
                             (make-instance 'parser-possibility
                                            :tree ,result-form
                                            :suffix (suffix-of ,result)))
                           (setf ,continuation nil)))))))))))

(defmacro named-seq? (&rest parser-descriptions)
  "Parser: This is similar to MDO, except that constructed parsers cannot depend on the results of
previous ones and the final form is not used as a parser, but is automatically used to construct the
result. All names bound using the (<- name parser) construct are only available in that final form.

This parser generator is useful when full generality of MDO is not necessary, as it is implemented
non-recursively and has better memory performance."
  `(%named-seq? seq-list? ,@parser-descriptions))

(defparameter *cut-tag* nil)

(defun tag? (parser format-control &rest format-arguments)
  "Parser modifier: add formatted string to tag stack for given parser."
  (let ((tag (apply #'format nil format-control format-arguments)))
    (with-parsers (parser)
      #'(lambda (inp)
          (if *cut-tag*
              (let ((tag-stack *tag-stack*)
                    (continuation (funcall parser inp)))
                #'(lambda ()
                    (let ((*tag-stack* tag-stack))
                      (funcall continuation))))
              (let ((*tag-stack* (cons tag *tag-stack*)))
                ;; bind *tag-stack* to mark any non-lazy parser results (usually first result will be
                ;; evaluated strictly), save it and apply it to continuation
                (let ((tag-stack *tag-stack*)
                      (continuation (funcall parser inp)))
                  #'(lambda ()
                      (let ((*tag-stack* tag-stack))
                        (funcall continuation))))))))))

(defun cut-tag? (parser format-control &rest format-arguments)
  "Parser modifier: add formatted string to tag stack for given parser, suppressing all lower level
parsers."
  (let ((tag (apply #'format nil format-control format-arguments)))
    (with-parsers (parser)
      #'(lambda (inp)
          (let ((*cut-tag* t)
                (*tag-stack* (cons tag *tag-stack*)))
            ;; bind *tag-stack* to mark any non-lazy parser results (usually first result will be
            ;; evaluated strictly), save it and apply it to continuation
            (let ((tag-stack *tag-stack*)
                  (continuation (funcall parser inp)))
              #'(lambda ()
                  (let ((*tag-stack* tag-stack)
                        (*cut-tag* t))
                    (funcall continuation)))))))))

(def-cached-parser context?
  "Parser: return current context without consuming any input"
  (define-oneshot-result inp is-unread
    (make-instance 'parser-possibility :tree inp :suffix inp)))

(def-cached-parser end?
  "Parser: matches end of input, returns t"
  (tag?
   (define-oneshot-result inp is-unread
     (update-front-context inp)
     (when (end-context-p inp)
       (make-instance 'parser-possibility :tree t :suffix inp)))
   "end of input"))

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
                                            ;; use map to gurantee the result type is handled the
                                            ;; same way as in the case above
                                            :tree (map result-type #'identity nil)
                                            :suffix inp))))))))))))

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

(defun sepby1? (parser-item parser-separator)
  "Parser: accept at least one of parser-item separated by parser-separator"
  (named-seq? (<- x parser-item)
              (<- xs (many? (mdo parser-separator (<- y parser-item) (result y))))
              (cons x xs)))

(defun bracket? (parser-open parser-center parser-close)
  "Parser: accept parser-center bracketed by parser-open and parser-close"
  (named-seq? parser-open (<- xs parser-center) parser-close xs))

(defun sepby? (parser-item parser-separator)
  "Parser: accept zero or more of parser-item separated by parser-separator"
  (choice (sepby1? parser-item parser-separator) (result nil)))

;; since all intermediate results have to be kept anyway for backtracking, they might be just as
;; well be kept not on the stack, so chainl/r1? can be implemented in terms of between? as well

(defun sepby1-cons? (p op)
  "Parser: as sepby1, but returns a list of a result of p and pairs (op p). Mainly a component parser for chains"
  (with-parsers (p op)
    (let ((between-parser (between? (named-seq? (<- op-result op)
                                                (<- p-result p)
                                                (cons op-result p-result))
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
      (named-seq? (<- chain subparser)
                  (destructuring-bind (front . chain) chain
                    (iter (for left initially front
                               then (funcall op
                                             left
                                             right))
                          (for (op . right) in chain)
                          (finally (return left))))))))

(defun chainr1? (p op)
  "Parser: accept one or more p reduced by result of op with right associativity"
  (with-parsers (p op)
    (let ((subparser (sepby1-cons? p op)))
      (named-seq? (<- chain subparser)
                  (destructuring-bind (front . chain) chain
                    (iter (with chain = (reverse chain))
                          (with current-op = (car (car chain)))
                          (with current-right = (cdr (car chain)))
                          (for (op . right) in (cdr chain))
                          (setf current-right (funcall current-op right current-right)
                                current-op op)
                          (finally (return (funcall op front current-right)))))))))

(defun chainl? (p op v)
  "Parser: like chainl1?, but will return v if no p can be parsed"
  (choice
   (chainl1? p op)
   (result v)))

(defun chainr? (p op v)
  "Parser: like chainr1?, but will return v if no p can be parsed"
  (choice
   (chainr1? p op)
   (result v)))

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
                (when (or (null min)
                          (>= count min))
                  (return (make-instance 'parser-possibility
                                         :tree (map result-type #'tree-of (gather-nodes node))
                                         :suffix (suffix-of node))))))))))

(defun find-after? (p q)
  "Parser: Find q after some sequence of p, earliest matches first."
  (named-seq? (breadth? p nil nil nil)
              (<- result q)
              result))

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
  "Parser: Find q after some sequence of p, earliest match first. Return cons of list of p-results and q"
  (named-seq? (<- prefix (breadth? p nil nil result-type))
              (<- q-result q)
              (cons prefix q-result)))

(defun find? (q)
  "Parser: Find q, earliest match first."
  (find-after? (item) q))

(defun hook? (function p)
  "Parser: apply function to result of p"
  (named-seq? (<- result p) (funcall function result)))

(defun chook? (result p)
  "Parser: return result if p matches"
  (named-seq? p result))

(defun chookahead? (result p)
  "Parser: return result if p matches, but do no advance"
  (with-parsers (p)
    (define-oneshot-result inp is-unread
      (let ((p-result (funcall (funcall p inp))))
        (when p-result
          (make-instance 'parser-possibility :tree result :suffix inp))))))

(defun opt? (p)
  "Parser: result of p or nil"
  (choice p (result nil)))

(defun validate? (p validation-function &optional (pre-hook #'identity))
  "Parser: call validation-function on result of (funcall pre-hook p), fail if it returns nil,
otherwhise return it as a result"
  (mdo (<- p-result p)
       (let ((hooked (funcall pre-hook p-result)))
         (if (funcall validation-function hooked)
             (result hooked)
             (zero)))))

(defun except? (p q)
  "Parser: match p unless q matches."
  (let ((not-q-result (gensym)))
    (with-parsers (p q)
      (mdo (<- maybe-q-result (choice1 q (result not-q-result)))
        (if (eql maybe-q-result not-q-result)
            p
            (zero))))))

(defmacro named? (name &body body)
  "Parser macro: give BODY a NAME, so it can refer to itself without causing generator recursion."
  (with-unique-names (parser wrapped inp)
    `(let ((,wrapped (zero)))
       (let ((,name
              #'(lambda (,inp)
                  (funcall ,wrapped ,inp))))
         (let ((,parser
                ,@body))
           (setf ,wrapped ,parser)
           ,parser)))))

(defun nested? (p &key (min nil) (max nil) (result-type 'list) (bracket-left #\() (bracket-right #\)))
  "Parser: parse a sequence of p, like between?, but with p possibly nested in brackets."
  (if (and bracket-left bracket-right)
      (named? nested-parser
        (between? (choice (bracket? bracket-left nested-parser bracket-right)
                          p)
                  min
                  max
                  result-type))
      (between? p min max result-type)))

(defun expression? (term operators &optional (bracket-left nil) (bracket-right nil))
  "Parser: Reduce a sequence of terms with unary/binary operators with precedence.
 OPERATORS is a list of (op-parser :left/:right/:unary), where OP-PARSER is a parser consuming
 an operator and returning a reduction function. Highest precedence first."
  (with-parsers (term bracket-left bracket-right)
    (named? expr-parser
      (iter (for (op assoc) in operators)
            (for base initially (choice (bracket? bracket-left expr-parser bracket-right)
                                        term)
                 then (ecase assoc
                        (:left (chainl1? base op))
                        (:right (chainr1? base op))
                        (:unary (choice
                                 (named-seq? (<- op-fun op)
                                             (<- subexpr base)
                                             (funcall op-fun subexpr))
                                 base))))
            (finally (return base))))))
