(write (let ((x 10)) (+ x (* x x))))
(newline)

(write (let ((x 10) (y 1) (z (+ 2 3))) (* x y z)))
(newline)

(write
 (let ((x 10))
   (+ x (let ((x (+ x 1))) x))))
(newline)

(write
 (let ((x 10))
   (+ x
      (let ((x 20) (y (+ x 1 2 3))) (* x y)))))
(newline)

(write
 (let fact ((n 10))
   (if (<= n 0) 1 (* n (fact (- n 1))))))
(newline)

(write (let () (+ 1 2 3)))
(newline)

(write
 (let ((x 10))
   (+ x
      (let* ((x 20) (y (+ x 1 2 3))) (* x y)))))
(newline)

(write
 (let ((f (lambda (x) (+ x x x x x x x))))
   (+ (f 1) (f 2) (+ (f 3) (f 4) (f (+ 5 6 7))))))
(newline)

(write (let* ((x 1) (y (+ x 1)) (z (* x y))) (+ x y z)))
(newline)

(write
 (letrec* ((cube (lambda (x) (* x x x)))
	   (a (+ (cube 1) (cube 12)))
	   (b (+ (cube 9) (cube 10))))
   (write a)
   (newline)
   (write b)
   (newline)
   (= a b)))
(newline)

(write
 (letrec* ((fact (lambda (x) (if (<= x 0) 1 (* x (fact (- x 1))))))
	   (x (fact 2))
	   (y (fact 3))
	   (z (fact (+ x y))))
   z))
(newline)
