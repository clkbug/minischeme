; utils
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

(define (eval env exp)
;  (format #t "eval called with env[~a] and exp[~a]\n" env exp)
  (cond
   ((not (list? exp)) exp)
   ((eq? (car exp) 'write)
    (if (null? (cddr exp))
	(write (eval env (cadr exp)))
	(write (eval env (cadr exp)) (eval env (caddr exp)))))
   ((eq? (car exp) 'quote) (cadr exp))
   ((eq? (car exp) 'newline) (newline))
   ((eq? (car exp) 'cons) (cons (eval env (cadr exp)) (eval env (caddr exp))))
   ((eq? (car exp) 'car) (car (eval env (cadr exp))))
   ((eq? (car exp) 'cdr) (cdr (eval env (cadr exp))))
   (else
    (begin
      (format #t "failed to eval:\n~a\n" exp)
      (cons env '())))))

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

