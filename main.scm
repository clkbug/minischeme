(define (new-env) '(()))
(define (new-sub-env env) (cons '() env))
(define (env-add env sym exp)
  (let ((old (car env))
	(parents (cdr env)))
    (cons (cons (cons sym exp) old) parents)))

(define (env-find env sym)
  (define (sub-env-find env sym)
    (if (nil? env)
	#f
	(if (eq? sym (caar env))
	    (cdar env)
	    (env-find (cdr env) sym))))
  

(define (eval env exp)
  ; (format #t "eval called with env[~a] and exp[~a]\n" env exp)
  (cond
   ((not (list? exp)) (cons env exp))
   ((eq? (car exp) 'write)
    (if (null? (cddr exp))
	(write (eval env (cadr exp)))
	(write (eval env (cadr exp)) (eval env (caddr exp)))))
   (else
    (begin
      (format #t "failed to eval:\n~a\n" exp)
      (cons env '())))))

(define (main args)
  (let ((port (open-input-file (cadr args))))
    (let loop ((exp (read port)) (env (new-env)))
      (if (eof-object? exp)
	  '()
	  (loop (read port) (car (eval env exp)))))))

