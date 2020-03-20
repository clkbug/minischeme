(cond
 ((eq? (+ 1 2 3) 6) (write 0))
 (else (write 1)))

(write
 (cond
  ((eq? 1 5) 0)
  ((eq? 2 3) 1)
  ((eq? 3 (* 1 3)) 2)
  (else (write 10))))

(write
 (cond
  ((eq? 2 3) 0)
  (else (+ 1 2 3 4 5))))
