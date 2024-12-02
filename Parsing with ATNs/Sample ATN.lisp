(load "ATN Compiler")

(defun types (word)
  (case word
    ((do does did)		'(aux v))
    ((time times)		'(n   v))
    ((fly flies)		'(n   v))
    ((like)			'(v   prep))
    ((liked likes)		'(v))
    ((a an the)			'(det))
    ((arrow arrows)		'(n))
    ((i you he she him her it)	'(pron))))

(defnode mods/n
    (cat n mods/n
	 (pushr mods *))
    (up `(n-group ,(getr mods))))

(defnode mods
    (cat n mods/n
	 (setr mods *)))

(defnode pron
    (up `(np (pronoun ,(getr n)))))

(defnode np/pp
    (up `(up (det	,(getr det))
	     (modifiers ,(getr mods))
	     (noun	,(getr n))
			,(getr pp))))

(defnode np/n
    (up `(np (det	,(getr det))
	     (modifiers ,(getr mods))
	     (noun      ,(getr n))))
    (down pp np/pp
   	 (setr pp *)))

(defnode np/mods
    (cat n np/n
	 (setr n *)))

(defnode np/det
    (down mods np/mods
	  (setr mods *))
    (jump np/mods
          (setr mods nil)))

(defnode np
    (cat det np/det
	 (setr det *))
    (jump np/det
         (setr det nil))
    (cat pron pron
         (setr n *)))

(defnode pp/np
    (up `(pp (prep ,(getr prep))
	     (obj ,(getr op)))))

(defnode pp/prep
    (down np pp/np
	  (setr op *)))

(defnode pp
    (cat prep pp/prep
	 (setr prep *)))

(defnode v
    (up `(s (mood	,(getr mood))
	    (subj	,(getr subj))
	    (vcl (aux	,(getr aux))
		 (v	,(getr v)))))
    (down np s/obj
  	 (setr obj *)))

(defnode s/subj
    (cat v v
	 (setr aux nil)
	 (setr v *)))

(defnode s/obj
    (up `(s (mood	,(getr mood))
	    (subj	,(getr subj))
	    (vcl (aux	,(getr aux))
		 (v	,(getr v)))
	    (obj	,(getr obj)))))

(defnode s
    (down np s/subj
	  (setr mood 'decl)
	  (setr subj *))
    (cat v v
         (setr mood 'imp)
	 (setr subj '(np (pron you)))
	 (setr aux nil)
	 (setr v *)))
