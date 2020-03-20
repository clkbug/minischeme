(define-macro swap (lambda (a b) `(,b ,a)))
(swap 1 write)
(newline)

(define-macro (writeline x)
  `(begin (write ,x) (newline)))
(writeline (+ 1 2 3 4 5))

(define-macro (fact n)
  `(letrec ((fact-fn (lambda (n)
		       (if (= n 0)
			   1
			   (* n (fact-fn (- n 1)))))))
     (fact-fn ,n)))
(writeline (fact 0))
(writeline (fact 5))
(writeline (fact 7))

(define-macro my-let
  (lambda (binds . bodies)
    `((lambda ,(map car binds) ,@bodies) ,@(map cadr binds))))
; (writeline (macroexpand '(my-let ((x 1) (y 2) (z 3)) (+ x (* y z)))))
(writeline (my-let ((x 1) (y 2) (z 3)) (+ x (* y z))))
(writeline (my-let ((x 2))
		   (my-let ((x (+ x 1)) (y x)) (* x y))))
(my-let ((x 1) (y 2))
	(writeline x)
	(writeline y))

(define-macro (my-or . args)
  (if (null? args)
      '()
      (let ((sym (gensym)))
	`(let ((,sym ,(car args)))
	   (if ,sym
	       ,sym
	       (my-or ,@(cdr args)))))))

(write (macroexpand '(my-or (begin (write 0) #f) (= 0 1) (< 2 1) (begin (write 1) #f) (begin (write 2) #t) (begin (write 3) #t))))
(newline)


; (define-macro (my-let* binds . bodies)

(define-macro (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(writeline (fact 5))

