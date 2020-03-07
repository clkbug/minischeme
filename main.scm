(define (main args)
  (let loop ((lis (cdr args)))
    (if (null? lis)
	'()
	(begin
	  (call-with-input-file (car lis)
	    (lambda (port)
	      (let read-loop ((exp (read port)))
		(if (eof-object? exp)
		    '()
		    (format #t "~a\n" exp)))))
	  (loop (cdr lis))))))

