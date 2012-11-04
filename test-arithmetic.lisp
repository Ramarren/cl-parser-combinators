(in-package :parser-combinators-tests)

(defsuite* (arithmetic-test :in parser-combinators-tests))

;;; define parser for basic +-*/ arithmetic on natural numbers (no negatives)

;;; is having to define parser for every level of precedence cheating? easy enough to generate
;;; automatically though

(def-cached-parser factor-op?
  (choice1 (mdo (char? #\/) (result (curry #'list '/)))
           (mdo (char? #\*) (result (curry #'list '*)))))

(def-cached-parser factor*
  (chainl1* (nat*) (factor-op?)))

(def-cached-parser expr-op?
  (choice1 (mdo (char? #\+) (result (curry #'list '+)))
           (mdo (char? #\-) (result #'(lambda (x y)
                                        (list '+ x (list '- y)))))))

(def-cached-parser arith*
  (chainl1* (factor*) (expr-op?)))

(deftest test-arith1 ()
  (is (equal '(+ 1 (* 2 3))
             (tree-of (current-result (parse-string (arith*) "1+2*3"))))))

(defun make-random-arith-string (size)
  (coerce (cons (digit-char (1+ (random 9)))
                (iter (repeat size)
                      (collect (ecase (random 4)
                                 (0 #\+)
                                 (1 #\-)
                                 (2 #\*)
                                 (3 #\/)))
                      (collect (digit-char (1+ (random 9))))))
          'string))

(deftest test-random-arith ()
  (iter (repeat 100)
        (let ((arith-string (make-random-arith-string 100)))
          (is (handler-case
                  (= (eval (infix:string->prefix arith-string))
                     (eval (tree-of (current-result (parse-string (arith*) arith-string)))))
                (division-by-zero ()
                  (print 'division-by-zero)
                  t))))))

(defun measure-time (min-size max-size step &optional (parser (arith*)))
  (iter (for i from min-size to max-size by step)
        (print i)
        (for arith-string = (make-random-arith-string i))
        #+sbcl(sb-ext:gc :full t)
        (for start-time = (get-internal-real-time))
        (current-result (parse-string parser arith-string))
        (collect (list i
                       (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))))
