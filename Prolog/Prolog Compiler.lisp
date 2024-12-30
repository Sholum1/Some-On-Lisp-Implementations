(load "../Prolog/Prolog Interpreter")

(defvar *rules* nil)

(defun clear-rules ()
  (setf *rules* nil))

;; (defun gensym? (x)
;;   (and (symbolp x) (not (symbol-package x))))

(defun form (pat)
  (if (simple? pat)
      pat
      `(cons ,(form (car pat)) ,(form (cdr pat)))))

(defun fullbind (x b)
  (cond ((gensym? x) (aif2 (binding  x  b)
                           (fullbind it b)
                           (gensym)))
        ((atom x) x)
        (t (cons (fullbind (car x) b)
                 (fullbind (cdr x) b)))))

(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds)                    (match it y binds))
   ((binding y binds)                    (match x it binds))
   ((gensym? x)                          (values (cons (cons x y) binds) t))
   ((gensym? y)                          (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y)
	 (match (car x) (car y) binds))  (match (cdr x) (cdr y) it))
   (t                                    (values nil nil))))

(defun rule-fn (ant con)
  (with-gensyms (val win fact binds paths)
    `(=lambda (,fact ,binds ,paths)
       (declare (ignore ,paths))
       (with-gensyms ,(vars-in (list ant con) #'simple?)
         (multiple-value-bind
               (,val ,win)
             (match ,fact
               (list ',(car con)
                     ,@(mapcar #'form (cdr con)))
               ,binds)
           (if ,win
               ,(gen-query ant val paths)
               (fail)))))))

(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
                 (car ant)
                 `(and ,@ant))))
    `(length (conc1f *rules*
                     ,(rule-fn (rep_ ant) (rep_ con))))))

(defmacro with-inference (query &rest body)
  (let ((vars (vars-in query #'simple?))
        (gb (gensym)))
    `(with-gensyms ,vars
       (setq *paths* nil)
       (=bind (,gb) ,(gen-query (rep_ query) nil '*paths*)
	 (declare (ignore ,gb))
         (let ,(mapcar (lambda (v)
                         `(,v (fullbind ,v ,gb)))
                       vars)
           ,@body)
         (fail)))))

(defun gen-query (expr &optional binds paths)
  (case (car expr)
    (and  (gen-and  (cdr  expr) binds paths))
    (or   (gen-or   (cdr  expr) binds paths))
    (not  (gen-not  (cadr expr) binds paths))
    (lisp (gen-lisp (cadr expr) binds))
    (is   (gen-is   (cadr expr) (third expr) binds))
    (cut  `(progn   (setq *paths* ,paths)
		    (=values,binds)))
    (t    `(prove   (list ',(car expr)
			,@(mapcar #'form (cdr expr)))
		    ,binds *paths*))))

(=defun prove (query binds paths)
  (choose-bind r *rules*
    (=funcall r query binds paths)))

(defun gen-and (clauses binds paths)
  (if (null clauses)
      `(=values ,binds)
      (let ((gb (gensym)))
        `(=bind (,gb) ,(gen-query (car clauses) binds paths)
           ,(gen-and (cdr clauses) gb paths)))))

(defun gen-or (clauses binds paths)
  `(choose
    ,@(mapcar #'(lambda (c) (gen-query c binds paths))
              clauses)))

(defun gen-not (expr binds paths)
  (let ((gpaths (gensym)))
    `(let ((,gpaths *paths*))
       (setq *paths* nil)
       (choose (=bind (b) ,(gen-query expr binds paths)
                 (setq *paths* ,gpaths)
                 (fail))
               (progn
                 (setq *paths* ,gpaths)
                 (=values ,binds))))))

(defmacro with-binds (binds expr)
  `(let ,(mapcar #'(lambda (v) `(,v (fullbind ,v ,binds)))
                 (vars-in expr))
     ,expr))

(defun gen-lisp (expr binds)
  `(if (with-binds ,binds ,expr)
       (=values ,binds)
       (fail)))

(defun gen-is (expr1 expr2 binds)
  `(aif2 (match ,expr1 (with-binds ,binds ,expr2) ,binds)
         (=values it)
         (fail)))
