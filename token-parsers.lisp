(in-package :parser-combinators)

;; single character parsers

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

;; some usual lexical tokens

(def-cached-arg-parser whitespace? (&key (result-type nil) (accept-empty nil))
  "Parser: accept a sequence of whitespace characters."
  (gather-if* (rcurry #'member '(#\Space #\Newline #\	))
              :result-type result-type
              :accept-empty accept-empty))

(def-cached-parser word?
  "Parser: accept a string of alphabetic characters"
  (gather-if* #'alpha-char-p :result-type 'string))

;; naive implementation using monadic combinators, unfortunately rather slow
;; (defun int? ()
;;   "Parser: accept an integer"
;;   (mdo (<- f (choice (mdo (char? #\-) (result #'-)) (result #'identity)))
;;        (<- n (nat?))
;;        (result (funcall f n))))

(def-cached-arg-parser nat? (&optional (radix 10))
  "Parser: accept natural numbers"
  (named-seq* (<- number (gather-if* #'digit-char-p :result-type 'string))
   (parse-integer number)))

(def-cached-arg-parser int? (&optional (radix 10))
  "Parser: accept an integer, return as integer."
  (named-seq*
   (<- sign (choice (between* #\- 0 1)
                    (between* #\+ 0 1)))
   (<- number (nat?))
   (let ((sign (if (or (null sign) (eql (car sign) #\+))
                   1
                   -1)))
     (* sign number))))

(def-cached-arg-parser quoted? (&key (quote-char #\")
                                     (left-quote-char nil)
                                     (right-quote-char nil)
                                     (escape-char #\\)
                                     (include-quotes t))
  "Parser: accept a string delimited with quote-char, possibly escaped by escape-char, possibly including quotation chars."
  (let* ((left-quote-char (or left-quote-char quote-char))
         (right-quote-char (or right-quote-char quote-char))
         (gather-end-condition (if escape-char
                                   (rcurry #'member (list right-quote-char escape-char))
                                   (rcurry #'eql right-quote-char)))
         (quoted-quote (when escape-char
                         (format nil "~a~a" escape-char right-quote-char)))
         (internal-parser (if escape-char
                              (many* (choice1 (gather-if-not* gather-end-condition
                                                              :result-type 'string)
                                              (named-seq* quoted-quote 'quote)))
                              (gather-if-not* gather-end-condition :result-type nil :accept-empty t))))
    (named-seq*
     (<- c1 (context?))
     left-quote-char
     (<- data internal-parser)
     right-quote-char
     (<- c4 (context?))
     (if include-quotes
         (context-interval c1 c4)
         (with-output-to-string (str)
           (iter (for datum in data)
                 (if (eql datum 'quote)
                     (princ right-quote-char str)
                     (princ datum str))))))))
