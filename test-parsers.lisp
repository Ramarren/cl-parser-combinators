(in-package :parser-combinator-tests)

(in-suite parser-combinator-tests)

(defsuite* parsers-tests)

;;; test for predefined parsers
;;; at least some of them, this is boring, write automatic genetor

(deftest test-char? ()
  (is (eql #\a (car (parse-string (char? #\a) "a"))))
  (is (null (parse-string (char? #\a) "b"))))

(deftest test-digit? ()
  (is (eql #\5 (car (parse-string (digit?) "5"))))
  (is (null (parse-string (digit?) "b"))))

(deftest test-lower? ()
  (is (eql #\a (car (parse-string (lower?) "a"))))
  (is (null (parse-string (lower?) "A"))))

(deftest test-upper? ()
  (is (eql #\A (car (parse-string (upper?) "A"))))
  (is (null (parse-string (upper?) "a"))))

(deftest test-alphanum? ()
  (is (eql #\A (car (parse-string (alphanum?) "A"))))
  (is (eql #\a (car (parse-string (alphanum?) "a"))))
  (is (eql #\1 (car (parse-string (alphanum?) "1"))))
  (is (eql #\5 (car (parse-string (alphanum?) "5"))))
  (is (null (parse-string (alphanum?) ","))))

(deftest test-word? ()
  (is (equal (list #\a #\b #\c)
	     (car (parse-string (word?) "abc")))))

(deftest test-string? ()
  (is (equal (list #\a #\b #\c)
	     (car (parse-string (string? (list #\a #\b #\c)) "abc")))))

(deftest test-many? ()
  (is (equal (list nil)
	     (parse-string (many? (letter?)) "   ")))
  (is (equal (list #\a #\b #\c)
	     (car (parse-string (many? (letter?)) "abc"))))
  (is (equal (list #\a #\b)
	     (cadr (parse-string (many? (letter?)) "abc")))))

(deftest test-many1? ()
  (is (equal nil
	     (parse-string (many1? (letter?)) "   ")))
  (is (equal (list #\a #\b #\c)
	     (car (parse-string (many1? (letter?)) "abc"))))
  (is (equal (list #\a #\b)
	     (cadr (parse-string (many1? (letter?)) "abc")))))

(deftest test-times? ()
  (is (equal nil
	     (parse-string (times? (letter?) 2) "a")))
  (is (equal (list #\a #\b #\c)
	     (car (parse-string (times? (letter?) 3) "abc"))))
  (is (equal (list #\a #\b)
	     (car (parse-string (times? (letter?) 2) "abc")))))

(deftest test-atleast? ()
  (is (equal nil
	     (parse-string (atleast? (letter?) 2) "a")))
  (is (equal (list #\a #\b #\c #\d #\e)
	     (car (parse-string (atleast? (letter?) 3) "abcde"))))
  (is (equal (list #\a #\b)
	     (cadr (parse-string (atleast? (letter?) 2) "abc")))))