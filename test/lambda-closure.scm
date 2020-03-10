(write ((((lambda (x)
	    (lambda (y)
	      (lambda (x) (+ x y
			     ((lambda (z) (* z z)) x))))) 1) 2) 3))
(newline)

(write ((lambda (x) (list x (list (quote quote) x)))
	(quote
	 (lambda (x) (list x (list (quote quote) x))))))
(newline)


