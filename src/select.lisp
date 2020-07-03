;;;; select.lisp

(in-package :gases)

(annot:enable-annot-syntax)

@export
(defmacro q-of (elem func quan)
  `(find-if
    #'(lambda (el)
	(and (string= (first el) ,elem)
	     (,func (second el) ,quan)))
    formula))

@export
@annot.doc:doc
"@b(Описание:) макрос @b(find-atoms)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-atoms (get-sp \"H2O\") (and (q-of \"H\"  =  2) (q-of \"O\" =  1) t))
 (find-atoms (get-sp \"H2O\") (and (q-of \"NA\" =  1) (q-of \"CL\" = 1) t))
@end(code)
"
(defmacro find-atoms (elem rule)
  `(let ((formula (sp-chemical-formula ,elem)))
     ,rule))

@export
@annot.doc:doc
"@b(Описание:) макрос @b(find-by-atoms) позволяет выполнять поиск 
веществ в базе данных по количеству атомов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-by-atoms (and (q-of \"C\" >= 2) (q-of \"H\" = 6) (q-of \"O\" = 1)))
 => (\"C2H5OH\" \"CH3OCH3\" \"C3H6O,propylox\" \"C3H6O,acetone\" \"C3H6O,propanal\"
 \"C6H5OH,phenol\")
@end(code)
 (find-by-atoms (and (q-of \"H\" >= 2) (q-of \"H\" <= 3) (q-of \"O\" = 1))) 
 => (\"HBOH\" \"HCHO,formaldehy\" \"H2BOH\" \"H2O\" \"NH2OH\" \"CH2OH\" \"CH3O\" \"CH2CO,ketene\"
     \"CH3CO,acetyl\" \"H2O(cr)\" \"H2O(L)\")
"
(defmacro find-by-atoms (rule)
  `(let ((rez nil))
     (maphash
      #'(lambda (key value)
	  (when
	      (find-atoms value ,rule)
	  (push key rez))) 
     *sp-db*)
     rez))
