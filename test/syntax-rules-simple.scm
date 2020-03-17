(define-syntax double
  (syntax-rules ()
    ((_ x) (* x x))))

(write (double 10))
(newline)

(define-syntax mul3
  (syntax-rules ()
    ((_ x y z) (* x y z))))

(write (mul3 2 3 5))
(newline)

(define-syntax inc
  (syntax-rules ()
    ((_ x) (+ x 1))
    ((_ x i) (+ x i))))

(write (inc 1))
(newline)
(write (inc 2 3))
(newline)

(define-syntax my-if-let1
  (syntax-rules ()
    ((_ var expr then)
     ((lambda (var) (if var then)) expr))
    ((_ var expr then else)
     ((lambda (var) (if var then else)) expr))))

(let ((x 1) (y 2))
  (my-if-let1 z (< x y) (begin (write z) (newline))))

(let ((x 2) (y 1))
  (my-if-let1 z (< x y) '() (write (double (double x)))))
(newline)
