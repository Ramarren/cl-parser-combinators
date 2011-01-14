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

(defun collapse-ops (op-tree)
  (labels ((collapse-top (op-tree)
             (match op-tree
               ((_op (_op . _inner) . _outer)
                (collapse-ops (list* _op (mapcar #'collapse-ops (append _inner _outer)))))
               (_thing
                _thing))))
    (match op-tree
      ((_op (_op . _inner) . _outer)
       (collapse-top (list* _op (mapcar #'collapse-ops (append _inner _outer)))))
      ((_op . _args)
       (collapse-top (list* _op (mapcar #'collapse-ops _args))))
      (_thing
       _thing))))

(deftest test-random-arith ()
  (iter (repeat 100)
        (let ((arith-string (make-random-arith-string 100)))
          (is (handler-case
                  (= (eval (infix:string->prefix arith-string))
                     (eval (collapse-ops (tree-of (current-result (parse-string (arith*) arith-string))))))
                (division-by-zero ()
                  (print 'division-by-zero)
                  t))))))

(deftest test-print-random-arith (size)
  (let ((arith-string (make-random-arith-string size)))
    (print arith-string)
    (is (equal (print (infix:string->prefix arith-string))
               (print (collapse-ops (tree-of (current-result (parse-string (arith*) arith-string)))))))))

(defun measure-time (min-size max-size step &optional (parser (arith*)))
  (iter (for i from min-size to max-size by step)
        (print i)
        (for arith-string = (make-random-arith-string i))
        (sb-ext:gc :full t)
        (for start-time = (get-internal-real-time))
        (current-result (parse-string parser arith-string))
        (collect (list i
                       (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))))
