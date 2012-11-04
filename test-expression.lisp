(in-package :parser-combinators-tests)

(defsuite* (expression-test :in parser-combinators-tests))

;;; parser for basic +-*/ arithmetic with subexpressions and negation

(def-cached-parser unary-minus?
  (mdo (char? #\-) (result (curry #'list '-))))

(defun expr-arith? ()
  (expression? (nat*)
               `((,(unary-minus?) :unary)
                 (,(factor-op?) :left)
                 (,(expr-op?) :left))
               (char? #\()
               (char? #\))))

(defun expr-arith* ()
  (expression* (nat*)
               `((,(unary-minus?) :unary)
                 (,(factor-op?) :left)
                 (,(expr-op?) :left))
               (char? #\()
               (char? #\))))

(deftest test-expr1 ()
  (is (equal '(* 1 2)
             (tree-of (current-result (parse-string (expr-arith?) "1*2")))))
  (is (equal '(* 1 2)
             (tree-of (current-result (parse-string (expr-arith*) "1*2")))))
  (is (equal '(+ 1 (* 2 3))
             (tree-of (current-result (parse-string (expr-arith?) "1+2*3")))))
  (is (equal '(+ 1 (* 2 3))
             (tree-of (current-result (parse-string (expr-arith*) "1+2*3"))))))

(deftest test-expr2 ()
  (is (equal '(+ 1 (* 2 (- 3)))
             (tree-of (current-result (parse-string (expr-arith?) "1+2*-3")))))
  (is (equal '(+ 1 (* 2 (- 3)))
             (tree-of (current-result (parse-string (expr-arith*) "1+2*-3"))))))

(deftest test-expr3 ()
  (is (equal '(* (+ 1 2) 3)
             (tree-of (current-result (parse-string (expr-arith?) "(1+2)*3")))))
  (is (equal '(* (+ 1 2) 3)
             (tree-of (current-result (parse-string (expr-arith*) "(1+2)*3")))))
  (is (equal '(* 1 (+ 2 3))
             (tree-of (current-result (parse-string (expr-arith?) "1*(2+3)")))))
  (is (equal '(* 1 (+ 2 3))
             (tree-of (current-result (parse-string (expr-arith*) "1*(2+3)"))))))


(deftest test-random-expr-arith ()
  (iter (repeat 100)
        (let ((arith-string (make-random-arith-string 100)))
          (is (handler-case
                  (= (eval (infix:string->prefix arith-string))
                     (eval (tree-of (current-result (parse-string (expr-arith*) arith-string)))))
                (division-by-zero ()
                  (print 'division-by-zero)
                  t))))))
