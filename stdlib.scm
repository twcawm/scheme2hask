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
    (f (car lst) (foldr f accumulator (cdr lst )))
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

(define (unfold f init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold f (f init) pred ))
  )
)

(define (sum . l) (fold + 0 l))
(define (product . l) (fold * 1 l))
(define (and . l) (fold && #t l))
(define (or . l) (fold && #f l))

(define (max first . rest) (fold (lambda (prev curr) (if (> prev curr) prev curr)) first rest))
(define (min first . rest) (fold (lambda (prev curr) (if (< prev curr) prev curr)) first rest))

(define (length l) (fold (lambda (x y) (+ x 1)) 0 l))
(define (reverse l) (fold (flip cons) '() l))
(define (revwrong l) (foldr (flip cons) '() l ))

(define (mem-helper pred op) (lambda (accum next) (if (and (not accum) (pred (op next))) next accum)))
(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst) (fold (mem-helper (curry equal? obj) id) #f lst))

(define (map f lst) 
  (foldr 
    (lambda (x y) 
      (cons (f x) y)
      ) 
    '() 
    lst
  )
)

(define (filter pred lst)
  (foldr 
    (lambda (x y)
      (if (pred x)
        (cons x y)
        y
      )
    )
  '()
  lst
  )
)
         
