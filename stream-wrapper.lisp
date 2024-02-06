;;;; stream-wrapper.lisp

(in-package :fizzbuzz)

(defclass stream-wrapper (trivial-gray-streams:fundamental-character-input-stream)
  ((stream :accessor stream-wrapper-stream
	   :initarg :stream)
   (termination-seq :accessor stream-wrapper-termination-seq
		    :initarg :termination-seq)
   (termination-index :accessor stream-wrapper-termination-index
		      :initarg :termination-index)
   (buffer :accessor stream-wrapper-buffer
	   :initarg :buffer)))

(defun make-stream-wrapper (stream termination-seq)
  (make-instance 'stream-wrapper
		 :stream stream
		 :termination-index 0
		 :buffer (cl-speedy-queue:make-queue (length termination-seq))
		 :termination-seq termination-seq))

(defmacro with-stream ((stream-var &optional termination-seq-var termination-index-var buffer-var) stream &body body)
  (alexandria:with-gensyms (unused-seq-var unused-index-var unused-buffer-var)
    (let ((ignorables (loop repeat 1
			    unless termination-seq-var collect unused-seq-var
			      unless termination-index-var collect unused-index-var
				unless buffer-var collect unused-buffer-var)))
      `(with-accessors ((,stream-var stream-wrapper-stream)
			(,(or termination-seq-var unused-seq-var) stream-wrapper-termination-seq)
			(,(or termination-index-var unused-index-var) stream-wrapper-termination-index)
			(,(or buffer-var unused-buffer-var) stream-wrapper-buffer))
	   ,stream
	 (declare (ignore ,@ignorables))
	 ,@body))))

(defun fill-buffer (stream)
  (with-stream (s seq index buffer) stream
    (loop until (cl-speedy-queue:queue-full-p buffer) 
	  for next = (read-char s)
	  when (eq (aref seq index) next) do (incf index)
	    do (cl-speedy-queue:enqueue next buffer))))

(defun stream-wrapper-end-p (stream)
  (with-stream (s seq index buffer) stream
    (= (length seq) index)))

(defun stream-wrapper-read-char (stream)
  (with-stream (s seq index buffer) stream
    (fill-buffer stream)
    (unless (stream-wrapper-end-p stream)
      (cl-speedy-queue:dequeue buffer))))

(defmethod trivial-gray-streams:stream-read-char ((stream stream-wrapper))
  (or (stream-wrapper-read-char stream) :eof))

(defmethod trivial-gray-streams:stream-unread-char ((stream stream-wrapper) character)
  (error "Not supported"))

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream stream-wrapper))
  (or (stream-wrapper-read-char stream) :eof))

(defmethod trivial-gray-streams:stream-peek-char ((stream stream-wrapper))
  (with-stream (s seq index buffer) stream
    (fill-buffer stream)
    (unless (stream-wrapper-end-p stream)
      (cl-speedy-queue:queue-peek buffer))))

(defmethod trivial-gray-streams:stream-listen ((stream stream-wrapper))
  (with-stream (s seq index buffer) stream
    (fill-buffer stream)
    (unless (stream-wrapper-end-p stream)
      (not (cl-speedy-queue:queue-empty-p buffer)))))

(defmethod trivial-gray-streams:stream-read-line ((stream stream-wrapper))
  (coerce (loop for next = (stream-wrapper-read-char stream)
		until (or (not next) (char-equal #\Newline next))
		collect next)
	  'string))

(defmethod trivial-gray-streams:stream-clear-input ((stream stream-wrapper))
  nil)
