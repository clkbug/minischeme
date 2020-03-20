(define (writeline x)
  (write x)
  (newline))

(define (main args)
  (writeline (+ 1 2 3 4 5 6 7 8 9))
  (writeline (fib 5))
  (writeline args))

(define (fib n)
  (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
