(define get-operator 
	(lambda (op-symbol env)
		(cond
			((equal? op-symbol '+) +)
			((equal? op-symbol '-) -)
			((equal? op-symbol '*) *)
			((equal? op-symbol '/) /)
			(else (get-value op-symbol env))
		)
	)
)

(define define-stmt?
	(lambda (e)
		(and 
			(list? e) (equal? (car e) 'define) (symbol? (cadr e)) (= (length e) 3)
		)
	)
)

(define if-stmt?
	(lambda (e)
		(and
			(list? e) (equal? (car e) 'if) (= (length e) 4)	
		)
	)
)

(define let-stmt?
	(lambda (e)
		(and
			(list? e) (equal? (car e) 'let) (= (length e) 3)
		)
	)
)

(define letstar-stmt?
	(lambda (e)
		(and
			(list? e) (equal? (car e) 'let*) (= (length e) 3)
		)
	)
)

(define f-lst?
	(lambda (e)
		(and
			(list? e) (symbol? (car e)) (or (null? (cdr e)) (f-lst? (cdr e))) ;or part corresponds to reccurrence part // list of symbols
		)
	)
)

(define lambda-stmt?
	(lambda (e)
		(and
			(list? e) (equal? (car e) 'lambda) (f-lst? (cadr e)) (not (define-stmt? (caddr e)));first item should be lambda while 3rd item is not a define stmt
		)
	)
)

(define get-value
	(lambda (var env)
		(cond
			((null? env)
				(error "s8-interpret: unbound variable -->" var))
			((equal? (caar env) var)
				(cdar env))
			(else
				(get-value var (cdr env))
			)
		)
	)
)

(define extend-env 
	(lambda (var val old-env)
		(cons (cons var val) old-env)
	)
)

(define repl
	(lambda (env)
		(let* (
				(dummy1 (display "cs305> "))
				(expr (read))
				(new-env (if (define-stmt? expr)
					(extend-env (cadr expr) (s8-interpret (caddr expr) env) env)
						env
					)
				)
				(val (if (define-stmt? expr)
						(cadr expr)
						(s8-interpret expr env)
					 )
				)
				(dummy2 (display "cs305: "))
				(dummy3 (display val))
				(dummy4 (newline))
				(dummy4 (newline))
			)
		(repl new-env)
		)
	)
)
		
						
			
(define built-in-op?
	(lambda (op)
		(cond
			((eq? op '+) #t)
			((eq? op '*) #t)
			((eq? op '-) #t)
			((eq? op '/) #t)
			(else #f)
		)
	)
)


(define s8-interpret
	(lambda (e env)
		(cond
			((number? e) e)
			((symbol? e) (get-value e env))
			((not (list? e)) (error "s8-interpret: cannot evaluate -->" e))
			((if-stmt? e) 
				(if (eq? (s8-interpret (cadr e) env) 0)
					(s8-interpret (cadddr e) env)
					(s8-interpret (caddr e) env)
				)
			)
			((let-stmt? e) (let ((names (map car (cadr e))) (matches (map cadr (cadr e))));matches car of cadr of e with cadr of cadr of e (let statement)
								(let ((vals (map (lambda (init) (s8-interpret init env)) matches)))
									(let ((new-env (append (map cons names vals) env)))
										(s8-interpret (caddr e) new-env)
									)
								)
							)
			)
			((letstar-stmt? e) 
				(if (= (length (cadr e)) 1)
						   (let ((l (list 'let (cadr e) (caddr e)))) 
								(let ((names (map car (cadr l))) (inits (map cadr (cadr l))))
									(let ((vals (map (lambda (init) (s8-interpret init env)) inits)))
										(let ((new-env (append (map cons names vals) env)))
											(s8-interpret (caddr l) new-env)
										)
									)
								)
							)
						   (let ((first (list 'let (list (caadr e)))) (rest (list 'let* (cdadr e) (caddr e))))
								(let ((l (append first (list rest)))) 
									(let ((names (map car (cadr l))) (inits (map cadr (cadr l))))
										(let ((vals (map (lambda (init) (s8-interpret init env)) inits)))
											(let ((new-env (append (map cons names vals) env)))
												(s8-interpret (caddr l) new-env)
											)
										)
									)
								)
							)
				)
			)
			
			((lambda-stmt? e) e)
			
			(else 
				(cond 
					((lambda-stmt? (car e)) 
						(if (= (length (cadar e)) (length (cdr e)))
							(let* ((par (map s8-interpret (cdr e) (make-list (length (cdr e)) env))) (nenv (append (map cons (cadar e) par) env))) (s8-interpret (caddar e) nenv))
							(error "s8-interpret: number of formal parameters and actual parameters do not match")
						)
					)
					((built-in-op? (car e))
						(let ((operands (map s8-interpret (cdr e) (make-list (length (cdr e)) env))) (operator (get-operator (car e) env)))
							(cond ((and (equal? operator '+) (= (length operands) 0)) 0)
								((and (equal? operator '*) (= (length operands) 0)) 1)
								((and (or (equal? operator '-) (equal? operator '/)) (= (length operands) 0))
									(error "s8-interpret: need at least two operands for this operator -->" operator))
								(else (apply operator operands)))))
					(else (let* ((result (s8-interpret (list (get-value (car e) env) (cadr e)) env))) result))
				)
			)
		)
	)
)
		

(define cs305 (lambda () (repl '())))
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		