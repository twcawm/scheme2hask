(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))
(define (list . objs) objs)
(define (id obj) obj)
(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))

(define (curry f arg1) (lambda (arg) (apply f (cons arg1 (list arg )))))
(define (compose f g) (lambda (arg) (f (apply g arg))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? ( curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (not (odd? num)))

(define (foldr f accumulator lst)
  (if (null? lst)
    accumulator
    (f (car lst) (foldr f end (cdr lst )))
  )
)

(define foldl (lambda (f accumulator lst)
                (if (null? lst)
                  accumulator
                  (foldl f (f accumulator (car lst)) (cdr lst))
                )
              )
)

(define reduce foldr)
(define fold foldl)

