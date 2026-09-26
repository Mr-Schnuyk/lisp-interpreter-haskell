
(defun f (a b) (cond 
	((< a b) (- a b))
	(T (+ b a))
))

(print "abs")
(print (f 5 (-1)))

(print '(cdr '(b car (() 10))))

(defun g (a b) (print "G") (cond 
	((atom a) (cdr '(car b car (() 10))))
))

(print (g '() (-1)))

(print (g 'a 'b))
