(defpackage :csv-parser
  (:use :cl :parser-combinators :alexandria)
  (:export #:parse-csv #:parse-csv-file))

(in-package :csv-parser)

;;;; Example of using parser-combinators to implement parsing of comma-separated value tables as
;;;; specified by RFC4180 http://www.ietf.org/rfc/rfc4180.txt

;;; A parser combinator is fundamentally a function that returns a function which when applied to
;;; input context return a parsing result (that is, a parser). Typically parsers will be constructed
;;; by combining more basic parsers (hence combinators). Fundamental combinators deal with
;;; backtracking and constructing results.

;;; By convention parsers are named with symbols which names end with '?' for backtracking parsers,
;;; and '*' for non-backtracking version of parsers. If the parser cannot backtrack because it is
;;; unambiguous over the input then '?' should be the default, to indicate there is no backtracking
;;; version.

;;; To be more explicit: the parser can either be ambiguous, in which case there are inputs that
;;; executing the parser over which will yield several results, and in which case the '?' version
;;; should return all of them lazily, so that they can be monadically combined. The '*' version of
;;; that parser should only return one result (and then report a failure to match if another result
;;; is requested), which sometimes allows a lot of optimization and might prevent unnecessary
;;; backtracking, although of course this should be used only if it is certain that for the specific
;;; class of inputs considered the first match is the correct one. This is for example common for
;;; repetition parsers. If the repeated element cannot be a part of the following syntax and the
;;; elements themselves are unambigous, the repetition parser can just accumulate the elements
;;; without having to keep track of intermediate matches.

;;; For certain class of parsers there is only one result for all possible matching inputs (for
;;; example a simple string match, which either matches or not). In that case the '?' and '*'
;;; versions would be identical and there is no reason to define both of them. I decided to define
;;; only the '?' version. That is all convention of course, the combinator system can't and doesn't
;;; see the names of the generator/combiners anyway.

(defun field? ()
  ;; A field can be terminated by a comma or a carriage return, if final in the record. A
  ;; gather-if-not* parser has a more efficient implementation for strings than a possible
  ;; implementation using breadth? or many? with excluded characters.

  ;; Parser combinators can take arbitrary parameters that are not parser combinators themselves. In
  ;; this case :accept-end means than an end of input is a valid gathering terminator and
  ;; :accept-empty that empty fields are valid.
  (gather-if-not* (rcurry #'member (list #\, #\Return))
                  :result-type 'string
                  :accept-end t
                  :accept-empty t))

;;; Accepting both an an end of input and empty fields in the parsers constructed above leads to an
;;; issue, where if the CSV string is terminated by the line break (optional according to RFC4180)
;;; then it would create a spurious record with a single empty field in the parse.

;;; One way to resolve that is to fail the parse at end of input when attempting to locate a field.
;;; This is implemented in the EXCEPT? parser combinator, but it is a basic MDO example, so let us
;;; reimplement this here.
(defun real-field? ()
  ;;; MDO (monadic do, which is a macro providing sugar over monadic BIND) can be used to create a
  ;;; parser depending on the result of some previous parsers. While powerful, this can be
  ;;; expensive, as any parser combinators/constructor needs to be rerun with any arguments
  ;;; provided. To avoid this (trivial in this case) cost we create the field parser and bind it to
  ;;; a variable outside of MDO macro.
  (let ((field? (field?)))
   (mdo
     ;; Within MDO <- assigns to a variable visible to all following combinator expressions. We
     ;; cannot just use (<- end (end?)) since it would fail to match if not at the end triggering
     ;; alternative parsing choices above, or failing the entire parse. To avoid this it is wrapped
     ;; in choice1 combinator. RESULT is a primitive constructor which consumes no input and returns
     ;; its argument as a parsing result. In this case this means that the variable END is bound to
     ;; either T at the end of input or NIL if not.
     (<- end (choice1 (end?) (result nil)))
     ;; Now we can use the end variable in a simple IF (but in general any expression returning a
     ;; parser will work) to return either a primitive constructor ZERO, which causes an
     ;; unconditional failure and forces backtracking, or the field? parser, which continues normal
     ;; parsing.
     (if end
         (zero)
         field?))))

(defun quoted-field? (&optional (include-quotes nil))
  ;; Fields including commas and line breaks have to be quoted. Double quotes are escaped with
  ;; themselves. Appropriately parametrized quoted? parser combinator handles this.
  (quoted? :quote-char #\" :escape-char #\" :include-quotes include-quotes))

(defun record? (&optional (include-quotes nil))
  ;; A record is a series of fields separated by commas. SEPBY1? implements this.
  (sepby1? (choice1 (quoted-field? include-quotes)
                    (real-field?))
           #\,))

(defun csv? (&key (include-quotes nil) (line-terminator :crlf))
  ;; RFC4180 specifically requires CRLF record separators, but we can optionally allow more general ones.
  (let ((crlf (ecase line-terminator
                (:crlf (map 'string #'identity (list #\Return #\Linefeed)))
                (:cr #\Return)
                (:lf #\Linefeed)
                ((t) (choices1 (map 'string #'identity (list #\Return #\Linefeed))
                               #\Return
                               #\Linefeed)))))
    ;; NAMED-SEQ? uses a syntax similar to MDO, but the variables bound are not visible to parser
    ;; combinators inside it, which allows executing them when the higher level parser combinator is
    ;; ran, not during parsing. The final form must be the result form, which assembles the result
    ;; from the bound variables. Other results are discarded, but they affect the parsing since the
    ;; have to match for the whole parser to succeed.
    (named-seq? (<- records (sepby1? (record? include-quotes) crlf))
                ;; The optional line break terminating the CSV string.
                (opt* crlf)
                ;; The result form for NAMED-SEQ?.
                records)))

;;; PARSE-STRING (which is a synonym for PARSE-SEQUENCE, since parser-combinator can parse all
;;; sequences for which context methods are defined) executes a parser over input and lazily
;;; computes all possible results as requested. PARSE-STRING* tries to find a first (possibly
;;; limited to those which consume the whole input string) result and returns it. For cases where
;;; the input should be unambiguous PARSE-STRING* is more directly useful.
(defun parse-csv (csv-string &key (complete t) (line-terminator :crlf) (include-quotes nil))
  (parse-string* (csv? :include-quotes include-quotes :line-terminator line-terminator)
                 csv-string
                 :complete complete))

;;; While it would be possible to define context methods for files, parser-combinators requires
;;; random access to the parsed sequence and it is most likely better to just load the file into
;;; memory.
(defun parse-csv-file (csv-file &key (complete t) (line-terminator :crlf) (include-quotes nil))
  (parse-string* (csv? :include-quotes include-quotes :line-terminator line-terminator)
                 (read-file-into-string csv-file)
                 :complete complete))
