;;;; fizzler-lang.lisp

(in-package :fizzbuzz)

(defun parse-numeral (stream)
  ;;; Only integers are supported
  (parse-integer
   (coerce (loop for next = (peek-char nil stream)
		 while (digit-char-p next) 
		 collect (read-char stream))
	   'string)))

(defun parse-string (stream)
  (coerce (loop for next = (peek-char nil stream)
		while (and next (char/= #\Newline next))
		collect (read-char stream))
   'string))

(defstruct lexer
  (tokens (make-array 8 :adjustable t :fill-pointer 0)))

(defun lexer-append (lexer token)
  (vector-push-extend token (lexer-tokens lexer) (min 8 (* (array-dimension (lexer-tokens lexer) 0) 2))))

(defun split-vector (vec split-token)
  (coerce (loop for token across vec
		for index from 0
		for start = 0 then (if split index start)
		for split = (eq split-token token)
		when split collect (subseq vec start index)
		  when (and (not split)
			    (= index (1- (length vec))))
		    collect (subseq vec start (1+ index)))
	  'vector))

(defun transpile-fizzler (lexer)
  (let ((expressions (split-vector (lexer-tokens lexer) :expression-break))
	(i 0))
    (labels ((find-patterns ()
	       (loop with match = (list)
		     with no-match = (list)
		     while (< i (length expressions))
		     for expression = (aref expressions i)
		     while (case (aref expression 0)
			     (:pattern-match (push expression match))
			     (:null-pattern (push expression no-match)))
		     do (incf i)
		     finally (return (values (nreverse match) (nreverse no-match)))))

	     (transpile-lambda (expression)
	       (let ((fn (aref expression 0))
		     (arg (aref expression 1)))
		 (case fn
		   (:modulus (alexandria:with-gensyms (x)
			       `(lambda (,x)
				  (= 0 (mod ,x ,arg)))))
		   (t (error "Unknown lambda ~a" fn)))))

	     (transpile-effect (expression)
	       (alexandria:with-gensyms (x)
		 `(lambda (,x)
		    (declare (ignorable ,x))
		    ,@(loop for value across expression
			    collect
			    (case value 
			      (:free-variable x)
			      (t value))))))

	     (transpile-pattern (pattern)
	       (let ((expressions (split-vector pattern :pattern-match)))
		 (unless (= 3 (length expressions))
		   (error "Expect pattern to be in the form |<pattern>|<effect> but was ~a ~a" pattern expressions))
		 (values (transpile-lambda (aref expressions 1))
			 (transpile-effect (aref expressions 2)))))

	     (transpile-null-pattern (expression)
	       (let ((expressions (split-vector expression :null-pattern)))
		 (transpile-effect (aref expressions 1))))
	     
	     (transpile-patterns ()
	       (multiple-value-bind (match no-match) (find-patterns)
		 (alexandria:with-gensyms (input aggregator has-match)
		   `(lambda (,input ,aggregator)
		      (let (,has-match)
			(funcall ,aggregator
				 (concatenate
				  'string
				  ,@(loop for pattern in match
					  for (pattern-fn pattern-result) = (multiple-value-list (transpile-pattern pattern))
					  collect `(when (funcall ,pattern-fn ,input)
						     (setf ,has-match t)
						     (princ-to-string (funcall ,pattern-result ,input))))

				  ,@(loop for pattern in no-match
					  collect `(unless ,has-match
						     (princ-to-string
						      (funcall ,(transpile-null-pattern pattern) ,input)))))))))))
				  
	     (transpile-loop (expression)
	       (let ((start (aref expression 0))
		     (end (aref expression 2)))
		 (incf i)
		 (alexandria:with-gensyms (index aggregator)
		   `(lambda (,aggregator)
		      (loop for ,index from ,start to ,end
			    do (funcall ,(transpile-expression) ,index ,aggregator))))))

	     (transpile-expression ()
	       (let* ((expression (aref expressions i)))
		 (cond ((find :pattern-match expression)
			(transpile-patterns))
		       ((find :range-infix expression)
			(transpile-loop expression)))))
	     
	     (collect-expressions ()
	       (loop while (< i (length expressions))
		     for expression = (aref expressions i)
		     when (> (length expression) 0)
		       collect (transpile-expression))))
      (alexandria:with-gensyms (aggregator arg)
	`(let ((,aggregator (lambda (,arg)
			      (princ ,arg)
			      (terpri))))
	   (funcall ,@(collect-expressions)
		    ,aggregator))))))

(defun parse-fizzler (stream)
  (loop with lexer = (make-lexer)
	for next = (peek-char nil stream nil)
	while next
	do
	   (labels ((lex (token)
		      (lexer-append lexer token)
		      (read-char stream)))
	     (case next
	       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		(lexer-append lexer (parse-numeral stream)))
	       ((#\:) (lex :range-infix))
	       ((#\|) (lex :pattern-match))
	       ((#\~) (lex :null-pattern))
	       ((#\%) (lex :modulus))
	       ((#\.) (lex :free-variable))
	       ((#\Newline) (lex :expression-break))
	       ((#\Space (read-char stream)))
	       (t (lexer-append lexer (parse-string stream)))))
	finally (return (transpile-fizzler lexer))))

(defun dispatch-banger-lang (stream char)
  (declare (ignore char))
  (let ((t1 (read-char stream))
	(t2 (read-char stream)))
    (assert (eq #\! t1))
    (assert (eq #\! t2))
    (let ((language-name (read-line stream)))
      (unless (equalp "fizzler" language-name)
	(error "fizzler is the only supported banger-lang"))

      (let ((wrapper (make-stream-wrapper stream "!!!")))
	(parse-fizzler wrapper)))))

(defmacro enable-fizzler ()
  `(set-macro-character #\! 'dispatch-banger-lang))

