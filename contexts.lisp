(in-package :parser-combinators)

(defparameter *tag-stack* nil)

(defclass context-front ()
  ((context :accessor context-of :initarg :context :initform nil)
   (tags    :accessor tags-of    :initarg :tags    :initform nil)))

(defclass context-common ()
  ((length        :accessor length-of        :initarg :length        :initform 0)
   (front         :accessor front-of         :initarg :front         :initform (make-instance 'context-front))
   (cache         :accessor cache-of         :initarg :cache         :initform nil)
   (seen-postions :accessor seen-postions-of :initarg :seen-position :initform (make-hash-table))))

(defclass context ()
  ((common      :accessor common-of      :initarg :common)
   (position    :accessor position-of    :initarg :position    :initform 0)))

(defmethod cache-of ((context context))
  (cache-of (common-of context)))

(defmethod length-of ((context context))
  (length-of (common-of context)))

(defmethod front-of ((context context))
  (front-of (common-of context)))

(defmethod seen-positions-of ((context context))
  (seen-postions-of (common-of context)))

(defmethod (setf front-of) (new-value (context context))
  (setf (front-of (common-of context)) new-value))

(defmethod position-of ((context-front context-front))
  (position-of (context-of context-front)))

(defmacro copy-context (context class &rest additional-arguments)
  `(make-instance ,class
                  ,@(iter (with default = (gensym))
                          (for (initarg accessor) in
                               '((:common common-of)
                                 (:position position-of)))
                          (appending (when (eql (getf additional-arguments initarg default)
                                                default)
                                       `(,initarg (,accessor ,context)))))
                  ,@additional-arguments))

(defun note-position (context posn)
  (declare (type (integer 0) posn))
  (incf (gethash posn (seen-positions-of context) 0)))

(defgeneric context-peek (context))

(defgeneric make-context-at-position (base-context position))

(defmethod make-context-at-position :around ((context context) position)
  (let ((cache (cache-of context)))
    (note-position context position)
    (etypecase cache
      (null (call-next-method))
      (vector (or (aref cache position)
                  (setf (aref cache position)
                        (call-next-method))))
      (hash-table (or (gethash position cache)
                      (setf (gethash position cache)
                            (call-next-method)))))))

(defgeneric context-next (context)
  (:method  ((context context))
    (make-context-at-position context (1+ (position-of context)))))

(defgeneric context-equal (context1 context2)
  (:method ((context1 context) (context2 context))
    (or (eq context1 context2)
        (and (eq (common-of context1)
                 (common-of context2))
             (eql (position-of context1)
                  (position-of context2))))))

(defgeneric context-greater (context1 context2)
  (:method ((context1 context) (context2 context))
    (and (eq (common-of context1)
             (common-of context2))
         (> (position-of context1)
            (position-of context2)))))

(defgeneric update-front-context (context)
  (:method ((context context))
    (let ((front (front-of context)))
      (cond ((or (null (context-of front))
                 (context-greater context (context-of front)))
             (setf (context-of front) context
                   (tags-of front) (list *tag-stack*)))
            ((context-equal context (context-of front))
             (push *tag-stack* (tags-of front)))))))

(defmethod context-peek :after ((context context))
  (update-front-context context))

(defgeneric context-interval (context1 context2 &optional result-type)
  (:method :before ((context1 context) (context2 context) &optional result-type)
    (declare (ignore result-type))
    (assert (eql (common-of context1)
                 (common-of context2)))
    (assert (<= (position-of context1)
                (position-of context2))))
  (:method ((context1 context) (context2 context) &optional (result-type 'string))
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
  ((storage :accessor storage-of :initarg :storage)))

(defmethod make-context-at-position ((base-context list-context) position)
  (assert (> position (position-of base-context)))
  (assert (<= position (length-of base-context)))
  (if (= position (length-of base-context))
      (copy-context base-context 'end-context :position position)
      (copy-context base-context 'list-context
                    :storage (nthcdr (- position (position-of base-context)) (storage-of base-context))
                    :position position)))

(defmethod context-peek ((context list-context))
  (car (storage-of context)))

(defclass vector-context-common (context-common)
  ((storage :accessor storage-of :initarg :storage)))

(defclass vector-context (context)
  ())

(defmethod storage-of ((context vector-context))
  (storage-of (common-of context)))

(defmethod make-context-at-position ((base-context vector-context) position)
  (assert (<= position (length-of base-context)))
  (if (= position (length-of base-context))
      (copy-context base-context 'end-context :position position)
      (copy-context base-context 'vector-context :position position)))

(defmethod context-peek ((context vector-context))
  (aref (storage-of context) (position-of context)))

(defmethod context-interval ((context1 vector-context) (context2 vector-context) &optional (result-type 'string))
  (let ((storage (storage-of context1)))
    (coerce (subseq storage (position-of context1) (position-of context2)) result-type)))

(defmethod context-interval ((context1 vector-context) (context2 end-context) &optional (result-type 'string))
  (let ((storage (storage-of context1)))
    (coerce (subseq storage (position-of context1)) result-type)))

(defvar *default-context-cache* :vector)

(defun make-cache (cache-type length)
  (ecase cache-type
    ((nil) nil)
    (:vector (make-array (1+ length) :initial-element nil))
    (:hashtable (make-hash-table))))

(defgeneric make-context (sequence &optional cache-type))

(defmethod make-context ((list list) &optional (cache-type *default-context-cache*))
  (if (null list)
      (make-instance 'end-context :common (make-instance 'context-common))
      (make-instance 'list-context
                     :storage list
                     :common (make-instance 'context-common
                                            :length (length list)
                                            :cache (make-cache cache-type (length list))))))

(defmethod initialize-instance :around ((context context) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((rest (call-next-method)))
    (note-position context (slot-value rest 'position))
    rest))

(defmethod make-context ((vector vector) &optional (cache-type *default-context-cache*))
  (if (zerop (length vector))
      (make-instance 'end-context :common (make-instance 'vector-context-common))
      (make-instance 'vector-context
                     :common (make-instance 'vector-context-common
                                            :storage vector
                                            :length (length vector)
                                            :cache (make-cache cache-type (length vector))))))
