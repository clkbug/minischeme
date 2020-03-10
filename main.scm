; utils
(define (map f lis)
  (if (null? lis)
      '()
      (cons (f (car lis)) (map f (cdr lis)))))
(define (iter f lis)
  (define (iter-sub f lis ret)
    (if (null? lis)
	ret
	(begin
	  (iter-sub f (cdr lis) (f (car lis))))))
  (iter-sub f lis '()))
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
    
  (define (dispatch msg . args)
    (cond
     ((eq? msg 'add) (apply add args))
     ((eq? msg 'find) (apply env-find args))
     ((eq? msg 'update) (apply update args))
     (else (format #t "failed to dispatch: env, msg(~a)\n" msg))))
  dispatch)

(define (make-closure env params bodies)
  (let ((clos-env (make-env env)))
    (lambda args
      (iter2 (lambda (var val) (clos-env 'add var val)) params args)
      (iter (lambda (body) (eval clos-env body)) bodies))))


(define (eval env exp)
;  (format #t "eval called with env[~a] and exp[~a]\n" env exp)
  (cond
   ((symbol? exp) (let ((val (env 'find exp))) (if val (cdr val) (exit 1))))
   ((not (list? exp)) exp)
   ((eq? (car exp) 'write)
    (if (null? (cddr exp))
	(write (eval env (cadr exp)))
	(write (eval env (cadr exp)) (eval env (caddr exp)))))
   ((eq? (car exp) 'quote) (cadr exp))
   ((eq? (car exp) 'list) (map (lambda (e) (eval env e)) (cdr exp)))
   ((eq? (car exp) 'newline) (newline))
   ((eq? (car exp) 'cons) (cons (eval env (cadr exp)) (eval env (caddr exp))))
   ((eq? (car exp) 'car) (car (eval env (cadr exp))))
   ((eq? (car exp) 'cdr) (cdr (eval env (cadr exp))))
   ((eq? (car exp) 'begin) (iter (lambda (e) (eval env e)) (cdr exp)))
   ((eq? (car exp) 'if) (eval env (if (eval env (cadr exp)) (caddr exp) (cadddr exp))))
   ((eq? (car exp) 'lambda)
    (let ((params (cadr exp))  ; "(lambda params bodies...)"
	  (bodies (cddr exp)))
      (make-closure env params bodies)))
    

   ; arithmetic operators
   ((eq? (car exp) '+) (apply + (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) '-) (apply - (map (lambda (e) (eval env e)) (cdr exp))))
   ((eq? (car exp) '*) (apply * (map (lambda (e) (eval env e)) (cdr exp))))
   (else
    (let ((f (eval env (car exp)))
	  (args (map (lambda (e) (eval env e)) (cdr exp))))
      (apply f args)
;      (format #t "failed to eval:\n~a\nf:~a\nargs:~a" exp (eval env (car exp)) (cdr exp))
      ))))

(define (main args)
  (define (top-level (make-env #f)))
  (if (null? (cdr args))
      '() ; テストのために引数なしのときは何もせずに終わる
      (let ((port (open-input-file (cadr args))))
	(let loop ((exp (read port)))
	  (eval top-level exp)
	  (if (eof-object? exp)
	      '()
	      (loop (read port)))))))

