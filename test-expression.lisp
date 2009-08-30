(in-package :parser-combinators-tests)

(in-suite parser-combinators-tests)

(defsuite* expression-test)

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
  (is (equal '(+ 1 (* 2 3))
             (tree-of (current-result (parse-string (expr-arith?) "1+2*3"))))))