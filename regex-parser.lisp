(defpackage :parser-combinators-cl-ppcre
    (:use :cl :parser-combinators :iterate)
  (:export #:regex* #:regex*-using-context))

(in-package :parser-combinators-cl-ppcre)

(defgeneric regex*-using-context (inp regex limit return-builder)
  (:method ((inp t) regex limit return-builder)
    (error "Parser regex* not implemented for this context type"))
  (:method ((inp vector-context) regex limit return-builder)
    (check-type (storage-of inp) string)
    (multiple-value-bind (match-start match-end regs-start regs-end)
        (cl-ppcre:scan regex (storage-of inp)
                       :start (position-of inp)
                       :end (if (and limit (<= limit (length (storage-of inp))))
                                (+ limit (position-of inp))
                                (length (storage-of inp))))
      (when match-start
        (let ((result
               (case return-builder
                 ((t) (subseq (storage-of inp) match-start match-end))
                 ((nil) nil)
                 (t (let ((regs (iter (for reg-start in-vector regs-start)
                                      (for reg-end in-vector regs-end)
                                      (collect (subseq (storage-of inp) reg-start reg-end)))))
                      (apply return-builder (subseq (storage-of inp) match-start match-end) regs))))))
          (if (= match-end (length (storage-of inp)))
              (values result (make-context-at-position inp (length (storage-of inp))))
              (values result (make-context-at-position inp :position match-end))))))))

(defun regex* (regex &key (limit nil) (return-builder t))
  "Non-backtracking parser: regular expression is applied to the input, and a result is constructed by return-builder (default will just return the match) from the result and submatches passed as strings. Passing regex-builder as nil will discard the result (use regex only to advance the input)."
  (let ((compiled-regex (cl-ppcre:create-scanner `(:sequence :start-anchor (:regex ,regex)))))
    (define-oneshot-result inp is-unread
      (multiple-value-bind (result new-input) (regex*-using-context inp compiled-regex limit return-builder)
        (when new-input
          (make-instance 'parser-possibility
                         :tree result
                         :suffix new-input))))))