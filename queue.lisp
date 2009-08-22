(in-package :parser-combinators)

(defclass queue ()
  ((head :accessor head-of :initform nil)
   (tail :accessor tail-of :initform nil)
   (size :accessor size-of :initform 0)))

(defun make-queue (&optional initial-contents)
  (let ((queue (make-instance 'queue)))
    (dolist (tk initial-contents queue)
      (push-back queue tk))))

(defgeneric peek (collection) (:documentation "Return top element without removing it"))

(defgeneric empty-p (collection) (:documentation "True if collection is empty"))

(defgeneric push-front (collection value) (:documentation "Push value to the front"))

(defgeneric pop-front (collection) (:documentation "Remove and return value from the front"))

(defgeneric push-back (collection value) (:documentation "Push value to the back"))

(defgeneric peek-back (collection) (:documentation "Return value from the back without removing it"))

(defmethod empty-p ((queue queue))
  (zerop (size-of queue)))

(defmethod peek ((queue queue))
  (if (empty-p queue)
      (values nil nil)
      (values (car (head-of queue)) t)))

(defmethod peek-back ((queue queue))
  (if (empty-p queue)
      (values nil nil)
      (values (car (tail-of queue)) t)))

(defmethod push-front ((queue queue) value)
  (setf (head-of queue) (cons value (head-of queue)))
  (unless (tail-of queue)
    (setf (tail-of queue) (head-of queue)))
  (incf (size-of queue))
  queue)

(defmethod pop-front ((queue queue))
  (if (empty-p queue)
      (values nil nil)
      (let ((front (car (head-of queue))))
	(setf (head-of queue) (cdr (head-of queue)))
	(unless (head-of queue)
	  (setf (tail-of queue) nil))
	(decf (size-of queue))
	(values front nil))))

(defmethod push-back ((queue queue) value)
  (let ((new-cons (cons value nil)))
    (when (tail-of queue)
      (setf (cdr (tail-of queue)) new-cons))
    (setf (tail-of queue) new-cons)
    (unless (head-of queue)
      (setf (head-of queue) new-cons)))
  (incf (size-of queue))
  queue)

(defun queue-to-list (queue)
  (head-of queue))
