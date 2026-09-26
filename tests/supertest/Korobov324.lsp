;
; Для интерпретации Лямбда-выражения нужно использовать функцию (calculation lambdaExp)
;


;редуцирование
(defun redux (exprs arg) (print "redux:: ") (print exprs) (print "arg:: ") (print arg) (reduxHelp (alpha (cdddr exprs) (cadr exprs)) arg (cadr exprs)))

(defun reduxHelp (exprs arg var) 
	(cond
		((null exprs) nil)
		((atom (car exprs))
			;редуцируемая переменная?
			(cond
				;да, подставляем
				((eq (car exprs) var) (cons arg (reduxHelp (cdr exprs) arg var)))
				;нет
				(T (cons (car exprs) (reduxHelp (cdr exprs) arg var)))
			)
		)
		(T (cons (reduxHelp (car exprs) arg var) (reduxHelp (cdr exprs) arg var)))
	)
)

;шаг НПР
(defun normalRstep (exprs) 
	(cond
		((null exprs) nil)
		;скобочки
		((null (car exprs)) (print "ERROR:: unexpected ()"))
		((atom (car exprs))
			(cond
				;если лямда, то альфа-преобрахование
				((eq (car exprs) '/) (cons '/ (cons (cadr exprs)(normalRstep (alpha (cddr exprs) (cadr exprs))))))
				(T (cons (car exprs) (normalRstep (cdr exprs)))) ; (a1 a2 ... (...
			)
		)
		
		((atom (caar exprs))
			;редукция возможна?
			(cond
				;нечем редуцировать: (...(/Y> ...))
				((null (cdr exprs)) (cons (normalRstep (car exprs)) ()))
				;скобочки
				((null (cadr exprs)) (print "ERROR:: unexpected ()"))
				;если лямда, то редуцируем
				;***ИСПРАВЛЕННАЯ ЧАСТЬ КОДА
				((and (atom (cddr exprs)) (eq (caar exprs) '/)) (append (redux (car exprs) (cadr exprs)) (cddr exprs)))
				((eq (caar exprs) '/) (cons (redux (car exprs) (cadr exprs)) (cddr exprs)))
				;***
				(T (cons (normalRstep (car exprs)) ()))
			)
		)
		;если появиись лишние скобочки
		((null (cadar exprs)) (cons (normalRstep (caar exprs)) (cdr exprs)))
		(T (cons (normalRstep (car exprs)) (cdr exprs))) ; ( ((...
	)
)

; вычисление лямбда-выражения
(defun calculation (exprs)
	(print "=============")
	(print "new step: ")
	(print exprs)
	(calculationHelp (normalRstep exprs) exprs)
)
	   
(defun calculationHelp (next exprs)
	(cond
		((eqList next exprs) exprs)
		(T (calculation next))
	)
)

; альфа-преобразование
(defun alpha (exprs var)
	(cond
		((null exprs) nil)
		((atom (car exprs)) 
			(cond
				;ситуация /x> ... /y>/x>...
				((and (eq (car exprs) '/) (eq (cadr exprs) var)) (renaming exprs var))
				(T (cons (car exprs) (alpha (cdr exprs) var)))
			)
		)
		;если внутри находится лямда-выражение с коллизией
		;((eq (cadar exprs) var) (cons (renaming (car exprs) var) (alpha (cdr exprs) var)))
		(T (cons (alpha (car exprs) var) (alpha (cdr exprs) var)))
	)
)

;переименование переменных
(defun renaming (exprs var)
    (print "---------")
	(print "Renaming a expression: ")
	(print exprs)
	(print "Renaming a variable: ")
	(print var)
	(renamingPrint (renamingHelpPrint exprs var (getNewName (toSet exprs) var 1)))
)

(defun renamingPrint (exprs)
	(print "Expression after renaming: ")
	(print exprs)
	exprs
)

(defun renamingHelpPrint (exprs var newvar)
    (print "to: ")
	(print newvar)
	(print "---------")
    (renamingHelp exprs var newvar)
)

(defun renamingHelp (exprs var newvar)
	(cond
		((null exprs) nil)
		((atom (car exprs)) 
		(cond
				((eq (car exprs) var) (cons newvar (renamingHelp (cdr exprs) var newvar)))
				(T (cons (car exprs) (renamingHelp (cdr exprs) var newvar)))
			)
		)
		(T (cons (renamingHelp (car exprs) var newvar) (renamingHelp (cdr exprs) var newvar)))
	)
)

;создаем новое имя без коллизии
(defun getNewName (exprsSet var N)
	(cond 
		((member (connection var N) exprsSet) (getNewName exprsSet var (+ N 1)))
		(T (connection var N))
	)
)

;преобразование списка во множество
(defun toSet (L)
	(toSetHelp L nil)
)

(defun toSetHelp (L res)
	(cond
		((null L) res)
		((atom (car L)) 
			(cond
				((member (car L) res) (toSetHelp (cdr L) res))
				(T (toSetHelp (cdr L) (cons (car L) res)))
			)
		)
		(T (toSetHelp (car L) (toSetHelp (cdr L) res)))
	)
)

;склейка атомов
;(intern (format nil "~A~A" atom1 atom2))
(defun connection (atom1 atom2)
	(intern atom1 atom2)
)

; эквивалентность списков
(defun eqList (L1 L2)
	(cond
		((null L1) (null L2))
		((null L2) nil)
		((atom (car L1)) 
			(cond
				((eq (car L1) (car L2)) (eqList (cdr L1) (cdr L2)))
				(T nil)
			)
		)
		((atom (car L2)) nil)
		((eqList (car L1) (car L2)) (eqList (cdr L1) (cdr L2)))
		(T nil)
	)
)

(print (calculation '(/ x > (/ y > / x > *   x   y ) x (/ z > b c))   ))
(print "^Final result^")


