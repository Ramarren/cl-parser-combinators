(in-package :parser-combinators)

(defclass context ()
  ((cache    :accessor cache-of    :initarg :cache :initform (make-hash-table))
   (storage  :accessor storage-of  :initarg :storage :initform nil)
   (position :accessor position-of :initarg :position :initform 0)
   (length   :accessor length-of   :initarg :length :initform 0)))

(defgeneric context-peek (context))
(defgeneric context-next (context))
(defgeneric make-context (sequence))

(defclass end-context (context)
  ())

(defmethod context-next ((context end-context))
  (error "Can't go past the end"))

(defmethod context-peek ((context end-context))
  (warn "Trying to peek past the end.")
  nil)

(defclass list-context (context)
  ())

(defmethod make-context ((list list))
  (if (null list)
      (make-instance 'end-context)
      (make-instance 'list-context :storage list :length (length list))))

(defmethod context-next ((context list-context))
  (with-accessors ((cache cache-of) (storage storage-of) (position position-of) (length length-of))
      context
    (let ((new-position (1+ position)))
      (or (gethash new-position cache)
          (setf (gethash new-position cache)
                (if (= new-position length)
                    (make-instance 'end-context
                                   :position new-position
                                   :length length
                                   :cache cache
                                   :storage nil)
                    (make-instance 'list-context
                                   :storage (cdr storage)
                                   :position new-position
                                   :length length
                                   :cache cache)))))))

(defmethod context-peek ((context list-context))
  (car (storage-of context)))

(defclass vector-context (context)
  ())

(defmethod make-context ((vector vector))
  (if (zerop (length vector))
      (make-instance 'end-context)
      (make-instance 'vector-context :storage vector :length (length vector))))

(defmethod context-next ((context vector-context))
  (with-accessors ((cache cache-of) (storage storage-of) (position position-of) (length length-of))
      context
    (let ((new-position (1+ position)))
      (or (gethash new-position cache)
          (setf (gethash new-position cache)
                (if (= new-position length)
                    (make-instance 'end-context
                                   :position new-position
                                   :length length
                                   :cache cache
                                   :storage nil)
                    (make-instance 'vector-context
                                   :storage storage
                                   :position new-position
                                   :length length
                                   :cache cache)))))))

(defmethod context-peek ((context vector-context))
  (aref (storage-of context) (position-of context)))
