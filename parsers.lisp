(in-package :parser-combinators)

(def-cached-arg-parser char? (character)
  "Parser: accept token eql to argument"
  (sat (curry #'eql character)))

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
  (assert (or (null min)
              (plusp min)))
  (assert (or (null max)
              (plusp max)))
  ;; gather results depth-first, longest first, ie. gather shorter on returning
  #'(lambda (inp)
      (let ((continuation-stack nil)
            (result-stack nil)
            (count 1)
            (state :next-result))
        (push (funcall parser inp) continuation-stack)
        #'(lambda ()
            (setf state :next-result)
            (iter (ecase state
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
                      (if result-stack
                          (let ((result
                                 (make-instance 'parser-possibility
                                                :tree (map result-type #'tree-of (reverse result-stack))
                                                :suffix (suffix-of (car result-stack)))))
                            (pop result-stack)
                            (return result))
                          (return nil)))))))))

(def-cached-parser word?
  "Parser: accept a string of alphabetic characters"
  (between? (letter?) nil nil 'string))

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
  (mdo (<- x parser-item)
       (<- xs (many? (mdo parser-separator (<- y parser-item) (result y))))
       (result (cons x xs))))

(defun bracket? (parser-open parser-center parser-close)
  "Parser: accept parser-center bracketed by parser-open and parser-close"
  (mdo parser-open (<- xs parser-center) parser-close (result xs)))

(defun sepby? (parser-item parser-separator)
  "Parser: accept zero or more of parser-item separated by parser-separator"
  (choice (sepby1? parser-item parser-separator) (result nil)))

(defun chainl1? (p op)
  "Parser: accept one or more p reduced by result of op with left associativity"
  (labels ((rest-chain (x)
             (choice
              (mdo (<- f op)
                   (<- y p)
                   (rest-chain (funcall f x y)))
              (result x))))
    (bind p #'rest-chain)))

(defun nat? ()
  "Parser: accept natural numbers"
  (chainl1? (mdo (<- x (digit?))
                 (result (digit-char-p x)))
            (result
             #'(lambda (x y)
                 (+ (* 10 x) y)))))

(defun chainr1? (p op)
  "Parser: accept one or more p reduced by result of op with right associativity"
  (bind p #'(lambda (x)
              (choice
               (mdo (<- f op)
                    (<- y (chainr1? p op))
                    (result (funcall f x y)))
               (result x)))))

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
