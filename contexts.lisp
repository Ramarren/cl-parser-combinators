(in-package :parser-combinators)

(defclass context ()
  ((sequence-id :accessor sequence-id-of :initarg :sequence-id :initform (gensym))
   (storage     :accessor storage-of     :initarg :storage     :initform nil)
   (position    :accessor position-of    :initarg :position    :initform 0)
   (length      :accessor length-of      :initarg :length      :initform 0)
   (cache       :accessor cache-of       :initarg :cache       :initform nil)))

(defmacro copy-context (context class &rest additional-arguments)
  `(make-instance ,class
                  ,@(iter (with default = (gensym))
                          (for (initarg accessor) in
                               '((:sequence-id sequence-id-of)
                                 (:storage storage-of)
                                 (:position position-of)
                                 (:length length-of)
                                 (:cache cache-of)))
                          (appending (when (eql (getf additional-arguments initarg default)
                                                default)
                                       `(,initarg (,accessor ,context)))))
                  ,@additional-arguments))

(defgeneric context-peek (context))

(defgeneric context-next (context))

(defmethod context-next :around (context)
  (let ((cache (cache-of context)))
    (etypecase cache
      (null (call-next-method))
      (vector (or (aref cache (position-of context))
                  (setf (aref cache (position-of context))
                        (call-next-method))))
      (hash-table (or (gethash (position-of context) cache)
                      (setf (gethash (position-of context) cache)
                            (call-next-method)))))))

(defgeneric context-equal (context1 context2)
  (:method ((context1 list-context) (context2 list-context))
    (or (eq context1 context2)
        (and (eq (sequence-id-of context1)
                 (sequence-id-of context2))
             (eql (position-of context1)
                  (position-of context2))))))

(defgeneric context-interval (context1 context2 &optional result-type)
  (:method ((context1 context) (context2 context) &optional (result-type 'string))
    (assert (eql (sequence-id-of context1)
                 (sequence-id-of context2)))
    (assert (<= (position-of context1)
                (position-of context2)))
    (if (= (position-of context1) (position-of context2))
        (coerce nil result-type)
        (coerce (iter (for c initially context1 then (context-next c))
                      (until (eq c context2))
                      (collect (context-peek c)))
                result-type))))

(defclass end-context (context)
  ())

(defgeneric end-context-p (context)
  (:method ((context t))
    nil)
  (:method ((context end-context))
    t))

(defmethod context-next ((context end-context))
  (error "Can't go past the end"))

(defmethod context-peek ((context end-context))
  (warn "Trying to peek past the end.")
  nil)

(defclass list-context (context)
  ())

(defmethod context-next ((context list-context))
  (let ((new-position (1+ (position-of context))))
    (if (= new-position (length-of context))
        (copy-context context 'end-context :storage nil :position new-position)
        (copy-context context 'list-context :storage (cdr (storage-of context)) :position new-position))))

(defmethod context-peek ((context list-context))
  (car (storage-of context)))

(defclass vector-context (context)
  ())

(defmethod context-next ((context vector-context))
  (let ((new-position (1+ (position-of context))))
    (if (= new-position (length-of context))
        (copy-context context 'end-context :position new-position :storage nil)
        (copy-context context 'vector-context :position new-position))))

(defmethod context-peek ((context vector-context))
  (aref (storage-of context) (position-of context)))

(defvar *default-context-cache* :vector)

(defun make-cache (cache-type length)
  (ecase cache-type
    ((nil) nil)
    (:vector (make-array length))
    (:hashtable (make-hash-table))))

(defgeneric make-context (sequence &optional cache-type))

(defmethod make-context ((list list) &optional (cache-type *default-context-cache*))
  (if (null list)
      (make-instance 'end-context)
      (make-instance 'list-context
                     :storage list
                     :length (length list)
                     :cache (make-cache cache-type (length list)))))

(defmethod make-context ((vector vector) &optional (cache-type *default-context-cache*))
  (if (zerop (length vector))
      (make-instance 'end-context)
      (make-instance 'vector-context
                     :storage vector
                     :length (length vector)
                     :cache (make-cache cache-type (length vector)))))
