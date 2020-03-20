(write ((((lambda (x)
	    (lambda (y)
	      (lambda (x) (+ x y
			     ((lambda (z) (* z z)) x))))) 1) 2) 3))
(newline)

; (fact 0)
(write (((lambda (x) (lambda (n) (if (= n 0) 1 (* n ((x x) (- n 1))))))
	 (lambda (x) (lambda (n) (if (= n 0) 1 (* n ((x x) (- n 1))))))) 0))
(newline)

; (fact 5)
(write (((lambda (x) (lambda (n) (if (= n 0) 1 (* n ((x x) (- n 1))))))
	 (lambda (x) (lambda (n) (if (= n 0) 1 (* n ((x x) (- n 1))))))) 5))
(newline)

; (fact 10)
(write (((lambda (x) (lambda (n) (if (= n 0) 1 (* n ((x x) (- n 1))))))
	 (lambda (x) (lambda (n) (if (= n 0) 1 (* n ((x x) (- n 1))))))) 10))
(newline)

; (fib 0)
(write (((lambda (x) (lambda (n) (if (<= n 1) 1 (+ ((x x) (- n 1)) ((x x) (- n 2))))))
	 (lambda (x) (lambda (n) (if (<= n 1) 1 (+ ((x x) (- n 1)) ((x x) (- n 2))))))) 0))
(newline)

; (fib 3)
(write (((lambda (x) (lambda (n) (if (<= n 1) 1 (+ ((x x) (- n 1)) ((x x) (- n 2))))))
	 (lambda (x) (lambda (n) (if (<= n 1) 1 (+ ((x x) (- n 1)) ((x x) (- n 2))))))) 3))
(newline)

; (fib 10)
(write (((lambda (x) (lambda (n) (if (<= n 1) 1 (+ ((x x) (- n 1)) ((x x) (- n 2))))))
	 (lambda (x) (lambda (n) (if (<= n 1) 1 (+ ((x x) (- n 1)) ((x x) (- n 2))))))) 10))
(newline)

; fact(10) with Z combinator
(write (((lambda (f)
	   ((lambda (X) (f (lambda (x) ((X X) x))))
	    (lambda (X) (f (lambda (x) ((X X) x))))))
	 (lambda (f)
	   (lambda (n)
	     (if (= n 0) 1 (* n (f (- n 1))))))) 10))
(newline)

; fib(10) with Z combinator
(write (((lambda (f)
	   ((lambda (X) (f (lambda (x) ((X X) x))))
	    (lambda (X) (f (lambda (x) ((X X) x))))))
	 (lambda (f)
	   (lambda (n)
	     (if (< n 2) 1 (+ (f (- n 1)) (f (- n 2))))))) 5))
(newline)
		

; quine
(write ((lambda (x) (list x (list (quote quote) x)))
	(quote
	 (lambda (x) (list x (list (quote quote) x))))))
(newline)


(write ((lambda (x y) (+ x y)) 2 3))
(newline)

(write ((lambda (x y z) (* x y z)) 4 5 6))
(newline)
