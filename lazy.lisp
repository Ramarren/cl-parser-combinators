(in-package :parser-combinators)

;;; emulate laziness as well, otherwhise any sort of recursion fails hard
(defclass promise ()
  ((thunk :accessor thunk-of :initarg :thunk)
   (value :accessor value-of :initform nil)))

(defmacro delay (&body body)
  `(make-instance 'promise
                  :thunk #'(lambda ()
                             ,@body)))

(defun force (promise)
  (with-accessors ((value value-of) (thunk thunk-of)) promise
    (if thunk
        (let ((real-value (funcall thunk)))
          (setf value real-value
                thunk nil)
          real-value)
        value)))
