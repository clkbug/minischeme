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
;  #?=lis1
;  #?=lis2
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

(define (eval env exp)
  (cond
   ((symbol? exp) (let ((val (env 'find exp))) (if val (cdr val) (error "not found: ~a in env" exp))))
   ((not (list? exp)) exp)
   (else
    (let ((f (eval env (car exp)))
	  (args (map (lambda (e) (eval env e)) (cdr exp))))
      (apply f args)))))

(define (main args)
  (let ((top-level (make-env #f)))
    (if (null? (cdr args))
	'() ; テストのために引数なしのときは何もせずに終わる
	(let ((port (open-input-file (cadr args))))
	  (let loop ((exp (read port)))
	    (eval top-level exp)
	    (if (eof-object? exp)
		'()
		(loop (read port))))))

    (if (top-level 'find 'main)
	(begin
	  (top-level 'add 'args (cdr args))
	  (eval top-level '(main args)))
	#f)))

