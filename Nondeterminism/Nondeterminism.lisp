(load "../Continuations/Continuations")

(defparameter *paths* nil)
(defconstant failsym '@)

(defmacro choose (&rest choices)
  (if choices
      `(progn
	 ,@(mapcar #'(lambda (c)
		       `(push #'(lambda () ,c) *paths*))
		   (reverse (cdr choices)))
	 ,(car choices))
      '(fail)))

(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))

(defun fail ()
  (if *paths*
      (funcall (pop *paths*))
      failsym))

(defun cb (fn choices)
  (if choices
      (progn
	(if (cdr choices)
	    (push #'(lambda () (cb fn (cdr choices)))
		  *paths*))
	(funcall fn (car choices)))
      (fail)))
