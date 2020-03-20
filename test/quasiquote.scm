(write '(1 2 3))
(newline)

(write (quasiquote 10))
(newline)

(define a 10)
(define l (list 1 2 3 4 5))

(write (quasiquote (unquote a)))
(newline)

(write (quasiquote (1 2 3 4 5)))
(newline)

(write (quasiquote ((unquote a) 2 3 4 5)))
(newline)

(write (quasiquote ((unquote l) (unquote-splicing l))))
(newline)

(write (quasiquote ((0 1 (2 3)) (4 (5 6) (7 (8 (9 (10 11 12) 13) 14) 15) 16))))
(newline)

(write (quasiquote (0 1 2 (unquote a))))
(newline)

(write (quasiquote (0 a (unquote a) l (unquote l))))
(newline)

(write (quasiquote (0 1 (unquote-splicing l))))
(newline)

(write (quasiquote ((0 1) (unquote-splicing (quote (1 2 (3 4) (unquote-splicing (5 6 7))))))))
(newline)

(define l2 (quote (1 2 (3 4) (unquote-splicing (5 6 7)))))
(write (quasiquote (0 1 (unquote-splicing l2))))
(newline)

(write (quasiquote (unquote ((lambda (x) (+ x x)) 100))))
(newline)

(write (quasiquote (3 . 4)))
(newline)

(write (quasiquote
	((1 2 . 3) . (4 5 . 6))))
(newline)
