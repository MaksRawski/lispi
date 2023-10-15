(define (s (lambda (x) (cons x nil))))
(define (peano (lambda (n) (cond ((eq n 0) nil) (T (s (peano (sum n -1))))))))
(define (p car))
(define (add (lambda (x y) (cond ((equal x nil) y) (T (add (p x) (s y)))))))
(define (mul (lambda (x y) (cond ((equal y (s nil)) x) (T (add x (mul x (p y))))))))
(define (fac (lambda (n) (cond ((equal n nil) (s nil)) (T (mul n (fac (p n))))))))

(define (seq (lambda (m n) (cond ((equal n m) (cons m nil)) (T (cons m (seq (s m) n)))))))
(define (maplist (lambda (fn x) (cond ((atom x) x) (T (cons (fn (car x)) (maplist fn (cdr x))))))))
(define (count (lambda (x) (cond ((equal x nil) 0) (T (sum 1 (count (car x))))))))
(define (fac1 (lambda (x) (count (fac x)))))

(maplist count (seq (peano 0) (peano 4)))
(maplist fac1 (seq (peano 0) (peano 4)))
