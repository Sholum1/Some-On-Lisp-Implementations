(load "../Query Compiler/Query Compiler")
(load "../Nondeterminism/Nondeterminism")

(defvar *rlist* nil)

(=defun prove-and (clauses binds)
  (if (null clauses)
      (=values binds)
      (=bind (binds) (prove-query (car clauses) binds)
	(prove-and (cdr clauses) binds))))

(=defun prove-or (clauses binds)
  (choose-bind c clauses
    (prove-query c binds)))

(=defun prove-not (expr binds)
  (let ((save-paths *paths*))
    (setq *paths* nil)
    (choose (=bind (b) (prove-query expr binds)
	      (declare (ignore b))
	      (setq *paths* save-paths)
	      (fail))
	    (progn
	      (setq *paths* save-paths)
	      (=values binds)))))

(=defun implies (r query binds)
  (let ((r2 (change-vars r)))
    (aif2 (match query (cdr r2) binds)
	  (prove-query (car r2) it)
	  (fail))))

(=defun prove-simple (query binds)
  (choose-bind r *rlist*
    (implies r query binds)))

(=defun prove-query (expr binds)
  (case (car expr)
    (and (prove-and    (cdr expr) binds))
    (or  (prove-or     (cdr expr) binds))
    (not (prove-not    (cdr expr) binds))
    (t   (prove-simple  expr      binds))))

(defmacro with-inference (query &body body)
  `(progn
     (setq *paths* nil)
     (=bind (binds) (prove-query ',(rep_ query) nil)
       (let ,(mapcar #'(lambda (v)
			 `(,v (fullbind ',v binds)))
		     (vars-in query #'atom))
	 ,@body
	 (fail)))))

(defun rep_ (x)
  (if (atom x)
      (if (eq x '_) (gensym "?") x)
      (cons (rep_ (car x)) (rep_ (cdr x)))))

(defun fullbind (x b)
  (cond ((varsym? x) (aif2 (binding x b)
			   (fullbind it b)
			   (gensym)))
	((atom x) x)
	(t (cons (fullbind (car x) b)
		 (fullbind (cdr x) b)))))

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
	    (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro conc1f (lst obj)
  `(_f nconc ,lst (list ,obj)))

(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
		 (car ant)
		 `(and ,@ant))))
    `(length (conc1f *rlist* (rep_ (cons ',ant ',con))))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun change-vars (r)
  (sublis (mapcar #'(lambda (v)
		      (cons v (symb '? (gensym))))
		  (vars-in r #'atom))
	  r))
