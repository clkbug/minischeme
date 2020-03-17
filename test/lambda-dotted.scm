(write ((lambda (x . y) (+ x (car y))) 1 2 3))
(newline)

(write ((lambda (x . y) (+ x (cadr y))) 1 2 3))
(newline)

(write ((lambda x x) 1 2 3 4 5))
(newline)
