;; Solution from
;; `https://groups.google.com/g/comp.lang.lisp/c/d5bsSR0o4Ps/m/6Hxj3TCvDHQJ'

(defmacro deflexical (var &optional init doc)
  `(progn (define-symbol-macro ,var (get ',var 'lexical))
	  (setf ,var ,init)
	  (setf (get ',var 'lexical-doc) ',doc)
	  ',var))

(deflexical cont #'values)

(defmacro =lambda (parms &body body)
  `#'(lambda (cont ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string "=" (symbol-name name)))))
    `(progn (defmacro ,name ,parms
	      `(,',f cont ,,@parms))
	    (defun ,f (cont ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((cont #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall cont ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn cont ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn cont ,@args))
