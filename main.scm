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
  (if (or (null? lis1) (null? lis2))
      '()
      (begin
	(f (car lis1) (car lis2))
	(iter2 f (cdr lis1) (cdr lis2)))))
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
  (define env '())
  (define (find-pred sym) (lambda (x) (eq? sym (car x))))
  (define (add sym val)
    (set! env (cons (cons sym val) env)))
  (define (env-find sym)
    (let ((result (find (find-pred sym) env)))
      (if result
	  result
	  (if parents (parents 'find sym) #f))))
  (define (update sym val)
    (let ((find-result (find (find-pred sym) env)))
      (if find-result
	  (set! env (cons (cons sym val) env))
	  (if parents (parents 'update sym val) #f))))
  (define (dump) (cons env parents))
  (define (dispatch msg . args)
    (cond
     ((eq? msg 'add) (apply add args))
     ((eq? msg 'find) (apply env-find args))
     ((eq? msg 'update) (apply update args))
     ((eq? msg 'dump) (dump))
     (else (format #t "failed to dispatch: env, msg(~a)\n" msg))))
  dispatch)

(define (make-closure env params bodies)
  (lambda args
    (let ((clos-env (make-env env)))
      (cond
       ((symbol? params)
	(begin
	  (clos-env 'add params args)
	  (iter (lambda (body) (eval clos-env body)) bodies)))
       ((list? params)
	(begin
	  (iter2 (lambda (var val) (clos-env 'add var val)) params args)
	  (iter (lambda (body) (eval clos-env body)) bodies)))
       (else (letrec ((iter2 (lambda (f lis1 lis2)
			       (if (pair? lis1)
				   (begin
				     (f (car lis1) (car lis2))
				     (iter2 f (cdr lis1) (cdr lis2)))
				   (f lis1 lis2)))))
	       (iter2 (lambda (var val) (clos-env 'add var val)) params args)
	       (iter (lambda (body) (eval clos-env body)) bodies)))))))


(define (expand-quasiquote exp)
  (cond
   ((not (pair? exp)) (quasiquote (quote (unquote exp))))
   ((eq? (car exp) 'unquote) (cadr exp))
   ((eq? (car exp) 'unquote-splicing) (error "unreachable: unquote-splicing out of list"))
   ((eq? (car exp) 'quasiquote)
    (expand-quasiquote
     (expand-quasiquote (cadr exp))))
   (else
    (quasiquote
     (append
      (unquote (expand-quasiquote-list (car exp)))
      (unquote (expand-quasiquote (cdr exp))))))))

(define (expand-quasiquote-list exp)
  (cond
   ((not (pair? exp)) (quasiquote (quote (unquote exp))))
   ((eq? (car exp) 'unquote) (quasiquote (list (unquote (cadr exp)))))
   ((eq? (car exp) 'unquote-splicing) (cadr exp))
   ((eq? (car exp) 'quasiquote)
    (expand-quasiquote-list (expand-quasiquote (cdr exp))))
   (else
    (quasiquote
     (list
      (append
       (unquote (expand-quasiquote-list (car exp)))
       (unquote (expand-quasiquote (cdr exp)))))))))
    
(define (eval-quasiquote env exp)
  (cond
   ((not (pair? exp)) exp)
   ((eq? (car exp) 'unquote) (eval env (cadr exp)))
   ((eq? (car exp) 'unquote-splicing) (error "unreachable! unquote-splicing out of list"))
   (else ;(error "not implemented yet"))))
    (apply append (map (lambda (e) (eval-quasiquote-list env e)) exp)))))

(define (eval-quasiquote-list env exp)
  (cond
   ((not (pair? exp)) (list exp))
   ((eq? (car exp) 'unquote) (list (eval env (cadr exp))))
   ((eq? (car exp) 'unquote-splicing) (eval env (cadr exp)))
   (else (list (eval-quasiquote env exp)))))

(define (eval env exp)
;  (format #t "eval called with env[~a] and exp[~a]\n" env exp)
  (cond
   ((symbol? exp) (let ((val (env 'find exp))) (if val (cdr val) (error "not found: ~a in env" exp))))
   ((not (list? exp)) exp)
   ((eq? (car exp) 'write)
    (if (null? (cddr exp))
	(write (eval env (cadr exp)))
	(write (eval env (cadr exp)) (eval env (caddr exp)))))
   ((eq? (car exp) 'quote) (cadr exp))
;   ((eq? (car exp) 'quasiquote) (eval env #?=(expand-quasiquote (cadr exp))))
   ((eq? (car exp) 'quasiquote) (eval-quasiquote env (cadr exp)))
   ((eq? (car exp) 'list) (map (lambda (e) (eval env e)) (cdr exp)))
   ((eq? (car exp) 'newline) (newline))
   ((eq? (car exp) 'cons) (cons (eval env (cadr exp)) (eval env (caddr exp))))
   ((eq? (car exp) 'car) (car (eval env (cadr exp))))
   ((eq? (car exp) 'cdr) (cdr (eval env (cadr exp))))
   ((eq? (car exp) 'cadr) (cadr (eval env (cadr exp))))
   ((eq? (car exp) 'begin) (iter (lambda (e) (eval env e)) (cdr exp)))
   ((eq? (car exp) 'if) (eval env (if (eval env (cadr exp)) (caddr exp) (cadddr exp))))
   ((eq? (car exp) 'define)
    (if (symbol? (cadr exp))
	(env 'add (cadr exp) (eval env (caddr exp)))
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
    (let ((binds (cadr exp))
	  (bodies (cddr exp)))
      (apply (make-closure env (map car binds) bodies)
	     (map (lambda (e) (eval env e)) (map cadr binds)))))


   ; arithmetic operators
   ((eq? (car exp) '+) (apply + (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) '-) (apply - (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) '*) (apply * (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) '=) (apply = (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) '<=) (apply <= (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) '>=) (apply >= (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) '<) (apply < (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) '>) (apply > (map (lambda (e) (eval env e)) (cdr exp))))
   
   ((eq? (car exp) 'eq?) (apply eq? (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) 'eqv?) (apply eqv? (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) 'equal?) (apply equal? (map (lambda (e) (eval env e)) (cdr exp))))

   ((eq? (car exp) 'not) (apply not (map (lambda (e) (eval env e)) (cdr exp))))
   ; ((eq? (car exp) 'or) (apply or (map (lambda (e) (eval env e)) (cdr exp))))
   ; ((eq? (car exp) 'and) (apply and (map (lambda (e) (eval env e)) (cdr exp))))

   ((eq? (car exp) 'list?) (apply list? (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) 'pair?) (apply pair? (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) 'apply) (apply apply (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) 'append) (apply append (map (lambda (e) (eval env e)) (cdr exp))))
   
   (else
    (let ((f (eval env (car exp)))
	  (args (map (lambda (e) (eval env e)) (cdr exp))))
      (apply f args)))))

(define (main args)
  (define top-level (make-env #f))
  (if (null? (cdr args))
      '() ; テストのために引数なしのときは何もせずに終わる
      (let ((port (open-input-file (cadr args))))
	(let loop ((exp (read port)))
	  (eval top-level exp)
	  (if (eof-object? exp)
	      '()
	      (loop (read port)))))))

