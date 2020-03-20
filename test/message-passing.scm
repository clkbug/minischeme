(define (writeline x) (write x) (newline))
; utils
(define (map f lis)
  (if (null? lis)
      '()
      (cons (f (car lis)) (map f (cdr lis)))))

(writeline (map (lambda (x) (writeline x) x) '(1 2 3 (4 5 6)  7 8 9)))
(writeline
 (map (lambda (x) (map (lambda (y) (* y 2)) x))
      '((1 2 3) (4 5 6 7) (8 9 10 11 12))))



(define (iter f lis)
  (letrec ((iter-sub
	    (lambda (f lis ret)
	      (if (null? lis) ret (iter-sub f (cdr lis) (f (car lis)))))))
    (iter-sub f lis '())))

(iter (lambda (x) (writeline x)) '(1 2 3 4 5 6 hoge fuga piyo pohe))

(define (iter2 f lis1 lis2)
  (cond
   ((null? lis1) '())
   ((not (pair? lis1)) (f lis1 lis2))
   (else (begin
	   (f (car lis1) (car lis2))
	   (iter2 f (cdr lis1) (cdr lis2))))))

(iter2 (lambda (x y) (write (+ x y)))
	       '(0 1 2 3 4 5 6)
	       '(6 5 4 3 2 1 0))
(newline)
(iter2 (lambda (x y) (x y))
       `(,(lambda (y) (writeline (+ y 2)))
	 ,(lambda (y) (writeline (* y 2)))
	 . ,(lambda (y) (writeline y)))
       '(99 99 100 100))

(define (find pred lis)
  (if (null? lis)
      #f
      (if (pred (car lis))
	  (car lis)
	  (find pred (cdr lis)))))

(writeline (find (lambda (x) (= (mod x 2) 0)) '(1 2 3 4 5)))
(writeline (find (lambda (x) #f) '(1 2 3 4)))


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

