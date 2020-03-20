; utils
(define (map f lis)
  (if (null? lis)
      '()
      (cons (f (car lis)) (map f (cdr lis)))))
(define (iter f lis)
  (letrec ((iter-sub
	    (lambda (f lis ret)
	      (if (null? lis) ret (iter-sub f (cdr lis) (f (car lis)))))))
    (iter-sub f lis '())))
(define (iter2 f lis1 lis2)
  (cond
   ((null? lis1) '())
   ((not (pair? lis1)) (f lis1 lis2))
   (else (begin
	   (f (car lis1) (car lis2))
	   (iter2 f (cdr lis1) (cdr lis2))))))
(define (find pred lis)
  (if (null? lis)
      #f
      (if (pred (car lis))
	  (car lis)
	  (find pred (cdr lis)))))

(define (error str . rest)
  (let ((message (apply format `(#f ,str ,@rest))))
    (format #t "error:\n\t~a\n" message))
  (exit 1))

(define (make-env parents)
  (let* ((env '())
	(find-pred (lambda (sym) (lambda (x) (eq? sym (car x)))))
	(add (lambda (sym val) (set! env (cons (cons sym val) env))))
	(env-find
	 (lambda (sym)
	   (let ((result (find (find-pred sym) env)))
	     (if result
		 result
		 (if parents (parents 'find sym) #f)))))
	(update
	 (lambda (sym val)
	   (let ((find-result (find (find-pred sym) env)))
	     (if find-result
		 (set! env (cons (cons sym val) env))
		 (if parents (parents 'update sym val) #f)))))
	(dump (lambda () (cons env parents)))
	(dispatch (lambda (msg . args)
		    (cond
		     ((eq? msg 'add) (apply add args))
		     ((eq? msg 'find) (apply env-find args))
		     ((eq? msg 'update) (apply update args))
		     ((eq? msg 'dump) (dump))
		     (else (format #t "failed to dispatch: env, msg(~a)\n" msg))))))
    dispatch))

(define (make-closure-with-names name env params bodies)
  (letrec ((closure
	    (lambda args
	      (let ((clos-env (make-env env)))
		; binds
		(cond
		 ((symbol? params) (clos-env 'add params args))
		 (else (iter2 (lambda (var val) (clos-env 'add var val)) params args)))
		; add self
		(clos-env 'add name closure)
		; eval bodies
		(iter (lambda (body) (eval body clos-env)) bodies)))))
    closure))


(define (make-closure env params bodies)
  (make-closure-with-names (gensym) env params bodies))


(define (expand-quasiquote exp)
  (cond
   ((not (pair? exp)) `',exp)
   ((eq? (car exp) 'unquote) (cadr exp))
   ((eq? (car exp) 'unquote-splicing) (error "unreachable here: unquote-splicing out of quasiquote"))
   ((eq? (car exp) 'quasiquote) (expand-quasiquote (expand-quasiquote (cadr exp))))
   (else `(append ,(expand-quasiquote-list (car exp)) ,(expand-quasiquote (cdr exp))))))

(define (expand-quasiquote-list exp)
  (cond
   ((not (pair? exp)) `'(,exp))
   ((eq? (car exp) 'unquote) `(list ,(cadr exp)))
   ((eq? (car exp) 'unquote-splicing) (cadr exp))
   ((eq? (car exp) 'quasiquote) (expand-quasiquote-list (expand-quasiquote (cadr exp))))
   (else `(list (append ,(expand-quasiquote-list (car exp)) ,(expand-quasiquote (cdr exp)))))))

(define (eval exp env)
;  #?=exp
  (cond
   ((symbol? exp) (let ((val (env 'find exp))) (if val (cdr val) (error "not found: ~a in env" exp))))
   ((not (list? exp)) exp)
   ((eq? (car exp) 'write)
    (if (null? (cddr exp))
	(write (eval (cadr exp) env))
					;	(write (eval (cadr exp) env) (eval (caddr exp) env))))
	#f))
   ((eq? (car exp) 'quote) (cadr exp))
   ((eq? (car exp) 'quasiquote) (eval (expand-quasiquote (cadr exp)) env))
   ((eq? (car exp) 'list) (map (lambda (expr) (eval expr env)) (cdr exp)))
   ((eq? (car exp) 'newline) (newline))
   ((eq? (car exp) 'cons) (cons (eval (cadr exp) env) (eval (caddr exp) env)))
   ((eq? (car exp) 'car) (car (eval (cadr exp) env)))
   ((eq? (car exp) 'cdr) (cdr (eval (cadr exp) env)))
   ((eq? (car exp) 'cadr) (cadr (eval (cadr exp) env)))
   ((eq? (car exp) 'cddr) (cddr (eval (cadr exp) env)))
   ((eq? (car exp) 'caddr) (caddr (eval (cadr exp) env)))
   ((eq? (car exp) 'begin) (iter (lambda (expr) (eval expr env)) (cdr exp)))
   ((eq? (car exp) 'if)
    (eval (if (eval (cadr exp) env)
	      (caddr exp)
	      (if (pair? (cdddr exp)) (cadddr exp) #f))
	  env))
   ((eq? (car exp) 'cond)
    (let loop ((conds-and-exps (cdr exp)))
      (if (not (null? conds-and-exps))
	  (let ((condition (caar conds-and-exps))
		(exp       (cadar conds-and-exps)))
	    (cond
	     ((or (eq? condition 'else) (eval condition env))
	      (eval exp env))
	     (else (loop (cdr conds-and-exps))))))))
   ((eq? (car exp) 'set!)
    (if (not (symbol? (cadr exp)))
	(error "unimplemented set! to non-variable")
	(env 'update (cadr exp) (eval (caddr exp) env))))
   ((eq? (car exp) 'define)
    (if (symbol? (cadr exp))
	(env 'add (cadr exp) (eval (caddr exp) env))
	(let* ((name-and-params (cadr exp))
	       (name (car name-and-params))
	       (params (cdr name-and-params))
	       (bodies (cddr exp))
	       (closure (make-closure env params bodies)))
	  (env 'add name closure))))

   ((eq? (car exp) 'lambda)
    (let ((params (cadr exp))  ; "(lambda params bodies...)"
	  (bodies (cddr exp)))
      (make-closure env params bodies)))

   ((eq? (car exp) 'let)
    (let* ((is-named-let? (symbol? (cadr exp)))
	   (name (if is-named-let? (cadr exp) (gensym)))
	   (binds (if is-named-let? (caddr exp) (cadr exp)))
	   (bodies (if is-named-let? (cdddr exp) (cddr exp))))
      (apply (make-closure-with-names name env (map car binds) bodies)
	     (map (lambda (expr) (eval expr env)) (map cadr binds)))))
   ((eq? (car exp) 'let*)
    (eval (let let*-expand ((binds (cadr exp))
			    (bodies (cddr exp)))
	    (if (null? binds)
		`(let () ,@bodies)
		`(let (,(car binds)) ,(let*-expand (cdr binds) bodies))))
	  env))
   ((eq? (car exp) 'letrec*)
    (eval (let letrec*-expand ((binds (cadr exp))
			       (bodies (cddr exp)))
	    (if (null? binds)
		`(let () ,@bodies)
		(let ((var (caar binds))
		      (val (cadar binds)))
		  `(let ((,var #f))
		     (set! ,var ,val)
		     ,(letrec*-expand (cdr binds) bodies)))))
	  env))
   ((eq? (car exp) 'letrec)
    (eval (let letrec-expand ((binds (cadr exp))
			      (bodies (cddr exp)))
	    `(let ,(map (lambda (bind) `(,(car bind) #f)) binds)
	       ,@(map (lambda (bind) `(set! ,(car bind) ,(cadr bind))) binds)
	       ,@bodies))
	  env))
			     


   ; arithmetic operators
   ((eq? (car exp) '+) (apply + (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) '-) (apply - (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) '*) (apply * (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) '=) (apply = (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) 'mod) (apply mod (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) '<=) (apply <= (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) '>=) (apply >= (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) '<) (apply < (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) '>) (apply > (map (lambda (expr) (eval expr env)) (cdr exp))))
   
   ((eq? (car exp) 'eq?) (apply eq? (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) 'eqv?) (apply eqv? (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) 'equal?) (apply equal? (map (lambda (expr) (eval expr env)) (cdr exp))))

   ((eq? (car exp) 'not) (apply not (map (lambda (expr) (eval expr env)) (cdr exp))))
   ; ((eq? (car exp) 'or) (apply or (map (lambda (expr) (eval expr env)) (cdr exp))))
   ; ((eq? (car exp) 'and) (apply and (map (lambda (expr) (eval expr env)) (cdr exp))))

   ((eq? (car exp) 'symbol?) (apply symbol? (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) 'null?) (apply null? (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) 'list?) (apply list? (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) 'pair?) (apply pair? (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) 'apply) (apply apply (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) 'append) (apply append (map (lambda (expr) (eval expr env)) (cdr exp))))

   ((eq? (car exp) 'read) (apply read (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) 'write) (apply write (map (lambda (expr) (eval expr env)) (cdr exp))))

   ((eq? (car exp) 'open-input-file) (apply open-input-file (map (lambda (expr) (eval expr env)) (cdr exp))))
   ((eq? (car exp) 'eof-object?) (apply eof-object? (map (lambda (expr) (eval expr env)) (cdr exp))))

   ((eq? (car exp) 'debug-print) (write (cons "debug-print" (cdr exp))))

   (else
    (let ((f (eval (car exp) env))
	  (args (map (lambda (expr) (eval expr env)) (cdr exp))))
      (apply f args)))))

(define (init-toplevel env) '())
  
  
(define (main args)
  (let ((top-level (make-env #f)))
    (if (null? (cdr args))
	'() ; テストのために引数なしのときは何もせずに終わる
	(let ((port (open-input-file (cadr args))))
	  (let loop ((exp (read port)))
	    (eval exp top-level)
	    (if (eof-object? exp)
		'()
		(loop (read port))))))

    (if (top-level 'find 'main)
	(begin
	  (top-level 'add 'args (cdr args))
	  (eval '(main args) top-level))
	#f)))

