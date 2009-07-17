(in-package :parser-combinators)

;;; greedy version of repetition combinators

(defmacro define-oneshot-result (inp is-unread &body body)
  `(function (lambda (,inp)
     (let ((,is-unread t))
       (make-parse-result
        #'(lambda ()
            (when ,is-unread
              (setf ,is-unread nil)
              ,@body)))))))

(defun many* (parser)
  "Parser: collect as many of first result of parser as possible"
  (define-oneshot-result inp is-unread
    (let ((final-result (iter (for result next (current-result (funcall parser inp-prime)))
                              (while result)
                              (for inp-prime initially inp then (suffix-of result))
                              (collect (tree-of result) into tree)
                              (finally (return (list tree inp-prime))))))
      (make-instance 'parser-possibility
                     :tree (car final-result)
                     :suffix (cadr final-result)))))


(defun many1* (parser)
  "Parser: accept as many as possible, and at least one, of parser"
  (mdo (<- x parser)
       (<- xs (many* parser))
       (result (cons x xs))))

(defun atleast* (parser count)
  "Parser: accept as many as possible and at least count of parser"
  (if (zerop count)
      (many* parser)
      (define-oneshot-result inp is-unread
        (let ((grab-result (current-result (funcall (many* parser) inp))))
          (when (>= (length (tree-of grab-result)) count)
            grab-result)))))


(defun atmost* (parser count)
  "Parser: accept as many as possible but at most count of parser"
  (define-oneshot-result inp is-unread
    (let ((final-result (iter (for result next (current-result (funcall parser inp-prime)))
                              (for i from 0)
                              (while (and result (< i count)))
                              (for inp-prime initially inp then (suffix-of result))
                              (collect (tree-of result) into tree)
                              (finally (return (list tree inp-prime))))))
      (make-instance 'parser-possibility
                     :tree (car final-result)
                     :suffix (cadr final-result)))))

(defun between* (parser min max)
  "Parser: accept as many as possible but between min and max of parser"
  (assert (>= max min))
  (if (zerop min)
      (atmost* parser max)
      (define-oneshot-result inp is-unread
        (let ((grab-result (current-result (funcall (atmost* parser max) inp))))
          (when (>= (length (tree-of grab-result)) min)
            grab-result)))))


(defun sepby1* (parser-item parser-separator)
  "Parser: accept as many as possible of parser-item separated by parser-separator, but at least one."
  (mdo (<- x parser-item)
       (<- xs (many* (mdo parser-separator
                          (<- y parser-item)
                          (result y))))
       (result (cons x xs))))

(defun sepby* (parser-item parser-separator)
  "Parser: accept as many as possible of parser-item separated by parser-separator."
  (choice1 (sepby1* parser-item parser-separator)
           (result nil)))

(defun chainl1* (p op)
  "Parser: accept as many as possible, but at least one of p, reduced by result of op with left associativity"
  (labels ((rest-chain (init-x)
             (define-oneshot-result inp is-unread
               (let ((final-result (iter (for f-result next (current-result (funcall op p-inp)))
                                         (while f-result)
                                         (for f-inp next (suffix-of f-result))
                                         (for p-result next (current-result (funcall p f-inp)))
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
  "Parser: accept natural number, consuming as many digits as possible"
  (chainl1* (mdo (<- x (digit?))
                 (result (digit-char-p x)))
            (result
             #'(lambda (x y)
                 (+ (* 10 x) y)))))

(defun int* ()
  "Parser: accept integer, consuming as many digits as possible"
  (mdo (<- f (choice1 (mdo (char? #\-) (result #'-)) (result #'identity)))
       (<- n (nat*))
       (result (funcall f n))))

(defun chainr1* (p op)
  "Parser: accept as many as possible, but at least one of p, reduced by result of op with right associativity"
  (bind p
    #'(lambda (init-x)
        (define-oneshot-result inp is-unread
          (let ((final-result
                 (iter (for f-result next (current-result (funcall op p-inp)))
                       (while f-result)
                       (for f-inp next (suffix-of f-result))
                       (while p-result)
                       (for p-result next (current-result (funcall p f-inp)))
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
  "Parser: like chainl1*, but will return v if no p can be parsed"
  (choice1
   (chainl1* p op)
   (result v)))

(defun chainr* (p op v)
  "Parser: like chainr1*, but will return v if no p can be parsed"
  (choice1
   (chainr1* p op)
   (result v)))

(defun find-after? (p q)
  "Parser: Find first q after some sequence of p."
  (define-oneshot-result inp is-unread
    (iter (for q-result next (current-result (funcall q inp-prime)))
          (until q-result)
          (for p-result next (current-result (funcall p inp-prime)))
          (while p-result)
          (for inp-prime initially inp then (suffix-of p-result))
          (finally (return (if q-result q-result nil))))))

(defun find? (q)
  "Parser: Find first q"
  (find-after? (item) q))
