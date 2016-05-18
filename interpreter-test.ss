(define (test-legacy)
  (let ([correct '(
                   19
                   13
                   773
                   (8 1 2 3 4 5 6 7)
                   (30 (29 27 24 20 15 9 2 3) 0)
                   3
                   (5 7)
                   (((a b (c () (d new (f g)) h)) i))
                   5
                   )]
        [answers
         (list
          (eval-one-exp '
           (let ([x 5] [y 3])
             (let ([z (begin (set! x (+ x y)) x)])
               (+ z (+ x y)))))
          (begin (reset-global-env)
                 (eval-one-exp '
                  (begin (define cde 5)
                         (define def (+ cde 2))
                         (+ def (add1 cde)))))
          (begin (reset-global-env)
                 (eval-one-exp '
                  (letrec ([f (lambda (n) (if (zero? n) 0 (+ 4 (g (sub1 n)))))]
                           [g (lambda (n) (if (zero? n) 0 (+ 3 (f (sub1 n)))))])
                    (g (f (g (f 5)))))))
          (begin (reset-global-env)
                 (eval-one-exp '
                  (define rotate-linear
                    (letrec ([reverse (lambda (lyst revlist)
                                        (if (null? lyst)
                                            revlist
                                            (reverse (cdr lyst)
                                                     (cons (car lyst) revlist))))])
                      (lambda (los)
                        (let loop ([los los] [sofar '()])
                          (cond [(null? los) los]
                                [(null? (cdr los)) (cons (car los) (reverse sofar '()))]
                                [else (loop (cdr los) (cons (car los) sofar))]))))))
                 (eval-one-exp '(rotate-linear '(1 2 3 4 5 6 7 8))))
          (begin (reset-global-env)
                 (eval-one-exp '
                  (let ([r 2] [ls '(3)] [count 7])
                    (let loop ()
                      (if (> count 0)
                          (begin (set! ls (cons r ls))
                                 (set! r (+ r count))
                                 (set! count (- count 1))
                                 (loop))
                          ))
                    (list r ls count))))
          (eval-one-exp '(apply apply (list + '(1 2))))
          (eval-one-exp '(apply map (list (lambda (x) (+ x 3)) '(2 4))))
          (eval-one-exp '
           (letrec ( [apply-continuation (lambda (k . list-of-values) (apply k list-of-values))]
                     [subst-left-cps
                      (lambda (new old slist changed unchanged)
                        (let loop ([slist slist] [changed changed] [unchanged unchanged])
                          (cond [(null? slist) (apply-continuation unchanged)]
                                [(symbol? (car slist))
                                 (if (eq? (car slist) old)
                                     (apply-continuation changed (cons new (cdr slist)))
                                     (loop (cdr slist)
                                           (lambda (changed-cdr)
                                             (apply-continuation changed (cons (car slist) changed-cdr)))
                                           unchanged))]
                                [else (loop (car slist)
                                            (lambda (changed-car)
                                              (apply-continuation changed (cons changed-car (cdr slist))))
                                            (lambda ()
                                              (loop (cdr slist)
                                                    (lambda (changed-cdr)
                                                      (apply-continuation changed (cons (car slist) changed-cdr)))
                                                    unchanged)))])))])
             (let ([s '((a b (c () (d e (f g)) h)) i)])
               (subst-left-cps 'new 'e s (lambda (changed-s)
                                           (subst-left-cps 'new 'q s
                                                           (lambda (wont-be-changed)
                                                             'whocares)
                                                           (lambda () (list changed-s))))
                               (lambda () "It's an error to get here")))))
          (eval-one-exp ' ((lambda () 3 4 5)))
          )])
    (display-results correct answers equal?)))

(define (test-simple-call/cc)
  (let ([correct '(
                   12
                   13
                   9
                   (#t)
                   9
                   (1 2 3)
                   )]
        [answers
         (list
          (eval-one-exp ' (+ 5 (call/cc (lambda (k) (+ 6 (k 7))))))
          (eval-one-exp ' (+ 3 (call/cc (lambda (k) (* 2 5)))))
          (eval-one-exp ' (+ 5 (call/cc (lambda (k) (or #f #f (+ 7 (k 4)) #f)))))
          (eval-one-exp '(list (call/cc procedure?)))
          (eval-one-exp ' (+ 2 (call/cc (lambda (k) (+ 3 (let* ([x 5] [y (k 7)]) (+ 10 (k 5))))))) )
          (eval-one-exp ' ((car (call/cc list)) (list cdr 1 2 3)) )
          )])
    (display-results correct answers equal?)))

(define (test-complex-call/cc)
  (let ([correct '(
                   9
                   1000
                   (6 7 8 9 100 11 12 13)
                   ((6 7 8 9 987654321 11 12 13))
                   (4)
                   9
                   )]
        [answers
         (list
          (begin
            (reset-global-env)
            (eval-one-exp '
             (define xxx #f))
            (eval-one-exp ' (+ 5 (call/cc (lambda (k)
                                            (set! xxx k) 2))))
            (eval-one-exp ' (* 7 (xxx 4))))
          (begin (eval-one-exp '
                  (define break-out-of-map #f))
                 (eval-one-exp '
                  (set! break-out-of-map (call/cc (lambda (k)
                                                    (lambda (x) (if (= x 7) (k 1000) (+ x 4)))))))
                 (eval-one-exp '(map break-out-of-map '(1 3 5 7 9 11)))
                 (eval-one-exp 'break-out-of-map))
          (begin (eval-one-exp ' (define jump-into-map #f))
                 (eval-one-exp ' (define do-the-map (lambda (x)
                                                      (map (lambda (v)
                                                             (if (= v 7)
                                                                 (call/cc (lambda (k) (set! jump-into-map k) 100))
                                                                 (+ 3 v)))
                                                           x))))
                 (eval-one-exp ' (do-the-map '(3 4 5 6 7 8 9 10))))
          (begin (eval-one-exp '
                  (define jump-into-map #f))
                 (eval-one-exp ' (define do-the-map (lambda (x)
                                                      (map (lambda (v) (if (= v 7)
                                                                           (call/cc (lambda (k) (set! jump-into-map k) 100))
                                                                           (+ 3 v))) x))))
                 (eval-one-exp ' (list (do-the-map '(3 4 5 6 7 8 9 10))))
                 (eval-one-exp ' (jump-into-map 987654321)))
          (eval-one-exp
           '(let ([y
                   (call/cc
                    (call/cc
                     (call/cc call/cc)))])
              (y list)
              (y 4)))
          (eval-one-exp '
           (+ 4 (apply call/cc (list (lambda (k) (* 2 (k 5))))))
           ))])
    (display-results correct answers equal?)))

(define (test-exit-list)
  (let ([correct '(
                   (6 7)
                   (5)
                   (3)
                   12
                   (12)
                   )]
        [answers
         (list
          (eval-one-exp ' (+ 4 (exit-list 5 (exit-list 6 7))) )
          (eval-one-exp ' (+ 3 (- 2 (exit-list 5))))
          (eval-one-exp ' (- 7 (if (exit-list 3) 4 5)))
          (eval-one-exp '(call/cc (lambda (k) (+ 100 (exit-list (+ 3 (k 12)))))))
          (eval-one-exp '(call/cc (lambda (k) (+ 100 (k (+ 3 (exit-list 12))))))))
         ])
    (display-results correct answers equal?)))

(define (test-set!-local-variables)
  (let ([correct '(
                   73
                   93
                   19
                   8
                   43
                   )]
        [answers
         (list
          (eval-one-exp '
           (let ([f #f] [x 3])
             (set! f (lambda (n)
                       (+ 3 (* n 10))))
             (set! x 7)
             (f x)))
          (eval-one-exp '((lambda (x) (set! x (+ x 1)) (+ x 2)) 90))
          (eval-one-exp '
           (let ([x 5] [y 3])
             (let ([z (begin (set! x (+ x y))
                             x)])
               (+ z (+ x y)))))
          (eval-one-exp '
           (let ([a 5])
             (if (not (= a 6))
                 (begin (set! a (+ 1 a))
                        (set! a (+ 1 a))) 3) (+ 1 a)))
          (eval-one-exp '
           (let ([f #f])
             (let ([dummy (begin (set! f (lambda (n) (+ 3 (* n 10))))
                                 3)])
               (f 4))))
          )])
    (display-results correct answers equal?)))

(define (test-simple-defines)
  (let ([correct '(
                   8
                   13
                   (12 14)
                   32
                   )]
        [answers
         (list
          (eval-one-exp ' (begin (define a 5) (+ a 3)))
          (eval-one-exp ' (begin (define c 5)
                                 (define d (+ c 2))
                                 (+ d (add1 c))))
          (eval-one-exp '
           (begin
             (define e 5)
             (let ([f (+ e 2)])
               (set! e (+ e f))
               (set! f (* 2 f))
               (list e f))))
          (eval-one-exp '
           (begin (define ff
                    (letrec ([ff (lambda (x)
                                   (if (= x 1)
                                       2
                                       (+ (* 2 x)
                                          (ff (- x 2)))))])
                      ff))
                  (ff 7)))
          )])
    (display-results correct answers equal?)))

(define (test-letrec-and-define)
  (let ([correct '(
                   55
                   773
                   )]
        [answers
         (list
          (begin (reset-global-env)
                 (eval-one-exp '
                  (letrec ([f (lambda (n)
                                (if (= n 0)
                                    0
                                    (+ n
                                       (f (sub1 n)))))])
                    (f 10))))
          (eval-one-exp '
           (letrec ([f (lambda (n)
                         (if (zero? n)
                             0
                             (+ 4 (g (sub1 n)))))]
                    [g (lambda (n)
                         (if (zero? n)
                             0
                             (+ 3 (f (sub1 n)))))])
             (g (f (g (f 5))))))
          )])
    (display-results correct answers equal?)))

(define (test-named-let-and-define)
  (let ([correct '(
                   120
                   120
                   (8 1 2 3 4 5 6 7)
                   987
                   16
                   (5 () (((4))) (3 2) 1)
                   )]
        [answers
         (list
          (eval-one-exp '
           (begin
             (define fact
               (lambda (n)
                 (let loop ((n n) (m 1))
                   (if (= n 0)
                       m
                       (loop (- n 1) (* m n))))))
             (fact 5)))
          (eval-one-exp '
           (let fact ((n 5) (m 1))
             (if (= n 0)
                 m
                 (fact (- n 1) (* m n)))))
          (begin (reset-global-env)
                 (eval-one-exp '
                  (define rotate-linear
                    (letrec ([reverse
                              (lambda (lyst revlist)
                                (if (null? lyst)
                                    revlist
                                    (reverse (cdr lyst)
                                             (cons (car lyst) revlist))))])
                      (lambda (los)
                        (let loop ([los los] [sofar '()])
                          (cond [(null? los) los]
                                [(null? (cdr los))
                                 (cons (car los) (reverse sofar '()))]
                                [else (loop (cdr los) (cons (car los) sofar))]))))))
                 (eval-one-exp '(rotate-linear '(1 2 3 4 5 6 7 8))))
          (begin (reset-global-env)
                 (eval-one-exp '
                  (define fib-memo
                    (let ([max 2] [sofar '((1 . 1) (0 . 1))])
                      (lambda (n)
                        (if (< n max)
                            (cdr (assq n sofar))
                            (let* ([v1 (fib-memo (- n 1))]
                                   [v2 (fib-memo (- n 2))]
                                   [v3 (+ v2 v1)])
                              (set! max (+ n 1))
                              (set! sofar (cons (cons n v3) sofar)) v3))))))
                 (eval-one-exp '(fib-memo 15)))
          (begin (reset-global-env)
                 (eval-one-exp '
                  (define f1 (lambda (x)
                               (f2 (+ x 1)))))
                 (eval-one-exp '
                  (define f2 (lambda (x) (* x x))))
                 (eval-one-exp '(f1 3)))
          (begin
            (reset-global-env)
            (eval-one-exp '
             (define ns-list-recur
               (lambda (seed item-proc list-proc)
                 (letrec ([helper (lambda (ls)
                                    (if (null? ls)
                                        seed
                                        (let ([c (car ls)])
                                          (if (or (pair? c)
                                                  (null? c))
                                              (list-proc (helper c)
                                                         (helper (cdr ls)))
                                              (item-proc c (helper (cdr ls)))))))])
                   helper))))
            (eval-one-exp '
             (define append
               (lambda (s t)
                 (if (null? s)
                     t
                     (cons (car s)
                           (append (cdr s) t))))))
            (eval-one-exp '
             (define reverse*
               (let ([snoc (lambda (x y)
                             (append y (list x)))])
                 (ns-list-recur '() snoc snoc))))
            (eval-one-exp '(reverse* '(1 (2 3) (((4))) () 5)))))])
    (display-results correct answers equal?)))

(define (test-set!-global-variables)
  (let ([correct '(
                   7
                   4
                   120
                   9
                   )]
        [answers
         (list
          (begin (reset-global-env)
                 (eval-one-exp '(define a 3))
                 (eval-one-exp '(set! a 7))
                 (eval-one-exp 'a))
          (begin (reset-global-env)
                 (eval-one-exp '(define a 3))
                 (eval-one-exp '(define f '()))
                 (eval-one-exp '(set! f (lambda (x) (+ x 1))))
                 (eval-one-exp '(f a)))
          (begin (reset-global-env)
                 (eval-one-exp '(define a 5))
                 (eval-one-exp '(define f '()))
                 (eval-one-exp '(set! f (lambda (x)
                                          (if (= x 0)
                                              1
                                              (* x (f (- x 1)))))))
                 (eval-one-exp '(f a)))
          (begin (reset-global-env)
                 (eval-one-exp '(define a 5))
                 (eval-one-exp '(let ([b 7]) (set! a 9)))
                 (eval-one-exp 'a))
          )])
    (display-results correct answers equal?)))


(define (test-order-matters!)
  (let ([correct '(
                   (30 (29 27 24 20 15 9 2 3) 0)
                   55
                   )]
        [answers
         (list
          (eval-one-exp '
           (let ([r 2] [ls '(3)] [count 7])
             (let loop ()
               (if (> count 0)
                   (begin (set! ls (cons r ls))
                          (set! r (+ r count))
                          (set! count (- count 1)) (loop)) ))
             (list r ls count)))
          (eval-one-exp '
           (begin
             (define latest 1)
             (define total 1)
             (or (begin
                   (set! latest (+ latest 1))
                   (set! total (+ total latest))
                   (> total 50))
                 (begin (set! latest (+ latest 1))
                        (set! total (+ total latest))
                        (> total 50))
                 (begin (set! latest (+ latest 1))
                        (set! total (+ total latest))
                        (> total 50))
                 (begin (set! latest (+ latest 1))
                        (set! total (+ total latest))
                        (> total 50))
                 (begin (set! latest (+ latest 1))
                        (set! total (+ total latest))
                        (> total 50))
                 (begin (set! latest (+ latest 1))
                        (set! total (+ total latest))
                        (> total 50))
                 (begin (set! latest (+ latest 1))
                        (set! total (+ total latest))
                        (> total 50))
                 (begin (set! latest (+ latest 1))
                        (set! total (+ total latest))
                        (> total 50))
                 (begin (set! latest (+ latest 1))
                        (set! total (+ total latest)) (> total 50))
                 (begin (set! latest (+ latest 1))
                        (set! total (+ total latest)) (> total 50))) total))
          )])
    (display-results correct answers equal?)))

(define (test-misc)
  (let ([correct '(
                   3
                   (5 7)
                   )]
        [answers
         (list
          (eval-one-exp '(apply apply (list + '(1 2))))
          (eval-one-exp '(apply map (list (lambda (x) (+ x 3)) '(2 4))))
          )])
    (display-results correct answers equal?)))

(define (test-ref)
  (let ([correct '(
                   (4 3)
                   (4 4)
                   (1 2 3)
                   ((1 2 3)(b b b))
                   )]
        [answers
         (list
          (eval-one-exp '
           (let ([a 3]
                 [b 4]
                 [swap! (lambda ((ref x) (ref y))
                          (let ([temp x])
                            (set! x y)
                            (set! y temp)))])
             (swap! a b)
             (list a b)))
          (eval-one-exp '
           (let ([a 3]
                 [b 4]
                 [swap (lambda ((ref x) y)
                         (let ([temp x])
                           (set! x y)
                           (set! y temp)))])
             (swap a b)
             (list a b)))
          (begin (reset-global-env)
                 (eval-one-exp '
                  (let* ([a '(1 2 3)]
                         [b ((lambda ((ref x)) x) a)])
                    (set! b 'foo) a)))
          (begin (reset-global-env)
                 (eval-one-exp ' (define x '(a a a)))
                 (eval-one-exp '(define y '(b b b)))
                 (eval-one-exp '(let ()
                                  ((lambda ((ref x) y)
                                     (set! x '(1 2 3))
                                     (set! y '(4 5 6)))
                                   x y)
                                  (list x y))))
          )])
    (display-results correct answers equal?)))

(define (test-basics)
  (let ([correct '(
                   (1 1 2 6 24 120)
                   40320
                   120
                   (#t #f #f #t)
                   )]
        [answers
         (list
          (eval-one-exp '
           (letrec ([fact (lambda (x)
                            (if (zero? x)
                                1
                                (* x (fact (- x 1)))))])
             (map fact '(0 1 2 3 4 5))))
          (eval-one-exp '
           (let f ([n 8] [acc 1])
             (if (= n 0)
                 acc
                 (f (sub1 n) (* acc n)))))

          (eval-one-exp '
           (let ([n 5])
             (let f ([n n] [acc 1])
               (if (= n 0)
                   acc
                   (f (sub1 n) (* acc n))))))

          (eval-one-exp '
           (letrec ([even? (lambda (n)
                             (if (zero? n)
                                 #t
                                 (odd? (- n 1))))]
                    [odd? (lambda (m)
                            (if (zero? m)
                                #f
                                (even? (- m 1))))])
             (list (odd? 3) (even? 3) (odd? 4) (even? 4))))      )])
    (display-results correct answers equal?)))

(define (test-answers-are-sets)
  (let ([correct '(
                   (k e b d a c)
                   ((3 a) (2 b)(3 b) (2 a) (1 a) (1 b))
                   )]
        [answers
         (list
          (eval-one-exp '
           (letrec ([union
                     (lambda (s1 s2)
                       (cond [(null? s1) s2]
                             [(member? (car s1) s2) (union (cdr s1) s2)]
                             [else (cons (car s1) (union (cdr s1) s2))]))]
                    [member? (lambda (sym ls)
                               (cond [(null? ls) #f]
                                     [(eqv? (car ls) sym) #t]
                                     [else (member? sym (cdr ls))]))])
             (union '(a c e d k) '(e b a d c))))
          (eval-one-exp '
           (letrec ([product
                     (lambda (x y)
                       (if (null? y)
                           '()
                           (let loop ([x x] [accum '()])
                             (if (null? x)
                                 accum
                                 (loop (cdr x)
                                       (append (map (lambda (s)
                                                      (list (car x) s))
                                                    y)
                                               accum))))))])
             (product '(1 2 3) '(a b))))
          )])
    (display-results correct answers sequal?-grading)))

(define (test-additional)
  (let ([correct '(
                   (8 6 5 4 3 2 1)
                   )]
        [answers
         (list
          (eval-one-exp '
           (letrec ([sort (lambda (pred? l)
                            (if (null? l) l
                                (dosort pred? l (length l))))]
                    [merge (lambda (pred? l1 l2)
                             (cond [(null? l1) l2]
                                   [(null? l2) l1]
                                   [(pred? (car l2) (car l1))
                                    (cons (car l2)
                                          (merge pred? l1 (cdr l2)))]
                                   [else (cons (car l1) (merge pred?
                                                               (cdr l1) l2))]))]
                    [dosort (lambda (pred? ls n)
                              (if (= n 1)
                                  (list (car ls))
                                  (let ([mid (quotient n 2)])
                                    (merge pred? (dosort pred? ls mid)
                                           (dosort pred?
                                                   (list-tail ls mid)
                                                   (- n mid))))))])
             (sort > '(3 8 1 4 2 5 6))))
          )])
    (display-results correct answers equal?)))


(define (test-subst-leftmost)
  (let ([correct '(
                   (((a b (c () (d new (f g)) h)) i))
                   )]
        [answers
         (list
          (eval-one-exp '
           (letrec (
                    [apply-continuation  (lambda (k . list-of-values)
                                           (apply k list-of-values))]
                    [subst-left-cps
                     (lambda (new old slist changed unchanged)
                       (let loop ([slist slist]
                                  [changed changed]
                                  [unchanged unchanged])
                         (cond
                          [(null? slist) (apply-continuation unchanged)]
                          [(symbol? (car slist))
                           (if (eq? (car slist) old)
                               (apply-continuation changed (cons new (cdr slist)))
                               (loop (cdr slist)
                                     (lambda (changed-cdr)
                                       (apply-continuation changed
                                                           (cons (car slist) changed-cdr)))
                                     unchanged))]
                          [else
                           (loop (car slist)
                                 (lambda (changed-car)
                                   (apply-continuation changed
                                                       (cons changed-car (cdr slist))))
                                 (lambda ()
                                   (loop (cdr slist)
                                         (lambda (changed-cdr)
                                           (apply-continuation changed
                                                               (cons (car slist) changed-cdr)))
                                         unchanged)))])))])
             (let ([s '((a b (c ()  (d e (f g)) h)) i)])
               (subst-left-cps 'new 'e s
                               (lambda (changed-s)
                                 (subst-left-cps 'new 'q s
                                                 (lambda (wont-be-changed) 'whocares)
                                                 (lambda () (list changed-s))))
                               (lambda () "It's an error to get here"))))))])
    (display-results correct answers equal?)))

(define (test-primitive-procedures-1)
  (let ([correct '((#t #t)
                   (#t #t #t)
                   (#t #t #t #f)
                   (#t #t #f #t #t #f)
                   (3 4 5)
                   (#t 5)
                   5
                   (a b c)
                   (#t #t #f)
                   )]
        [answers
         (list
          (eval-one-exp '
           (list (procedure? +)
                 (not (procedure? (+ 3 4)))))
          (eval-one-exp '
           (list (procedure? procedure?)
                 (procedure? (lambda(x) x))
                 (not (procedure? '(lambda (x) x)))))
          (eval-one-exp '
           (list (procedure? list)
                 (procedure? map)
                 (procedure? apply)
                 (procedure? #t)))
          (eval-one-exp '
           (map procedure?
                (list map car 3 (lambda(x) x) (lambda x x) ((lambda () 2)))))
          (eval-one-exp '(apply list (list 3 4 5)))
          (eval-one-exp ' (list (vector? (vector 3))
                                (vector-ref (vector 2 4 5)
                                            (vector-ref (vector 2 4 5) 0))))
          (eval-one-exp '(length '(a b c d e)))
          (eval-one-exp '(vector->list '#(a b c)))
          (eval-one-exp ' (list (procedure? list)
                                (procedure? (lambda (x y)
                                              (list (+ x y))))
                                (procedure? 'list)))

          )])
    (display-results correct answers equal?)))

(define (test-lambda-regression-tests)
  (let ([correct '(
                   6
                   12
                   154
                   720
                   (#t #t #f)
                   )]
        [answers
         (list
          (eval-one-exp '((lambda (x) (+ 1 x)) 5))
          (eval-one-exp '((lambda (x) (+ 1 x) (+ 2 (* 2 x))) 5))
          (eval-one-exp '
           ((lambda (a b)
              (let ([a (+ a b)]
                    [b (- a b)])
                (let ([f (lambda (a) (+ a b))])
                  (f (+ 3 a b)))))
            56
            17))
          (eval-one-exp '
           (((lambda (f)
               ((lambda (x)
                  (f (lambda (y) ((x x) y))))
                (lambda (x)
                  (f (lambda (y) ((x x) y))))))
             (lambda (g)
               (lambda (n)
                 (if (zero? n) 1 (* n (g (- n 1))))))) 6))
          (eval-one-exp '
           (let ([Y (lambda (f)
                      ((lambda (x) (f (lambda (y) ((x x) y))))
                       (lambda (x) (f (lambda (y) ((x x) y))))))]
                 [H (lambda (g) (lambda (x)
                                  (if (null? x) '()
                                      (cons (procedure? (car x))
                                            (g (cdr x))))))])
             ((Y H) (list list (lambda (x) x) 'list))))
          )])
    (display-results correct answers equal?)))

(define (test-lambda-with-variable-args)
  (let ([correct '(
                   (b c)
                   (9 2 1)
                   two
                   )]
        [answers
         (list
          (eval-one-exp '((lambda x (car x) (cdr x)) 'a 'b 'c))
          (eval-one-exp '((lambda (x y . z)
                            (cons (+ x y) (cdr z)))
                          5 4 3 2 1))
          (eval-one-exp ' ((lambda (x y . z)
                             (if (> x y)
                                 (car z)
                                 (cdr z))
                             (cadr z)) 5 4 'three 'two 'one))
          )])
    (display-results correct answers equal?)))

(define (test-syntactic-expansion)
  (let ([correct '(
                   7
                   6
                   8
                   8
                   6
                   0
                   #t
                   3
                   #f
                   #f
                   odd
                   even
                   out-of-range
                   0
                   2
                   (6)
                   (131072)
                   (3)
                   )]
        [answers
         (list
          (eval-one-exp '(cond [(< 4 3) 8] [(< 2 3) 7] [else 8]))
          (eval-one-exp '(cond [(< 4 3) 8] [(> 2 3) 7] [else 6]))
          (eval-one-exp '(cond [(> 4 3) 8] [(< 2 3) 7] [else 6]))
          (eval-one-exp '(cond [else 8]))
          (eval-one-exp '(let ([a (vector 3)])
                           (cond [(= (vector-ref a 0) 4) 5]
                                 [(begin (vector-set! a 0
                                                      (+ 1 (vector-ref a 0)))
                                         (= (vector-ref a 0) 4)) 6]
                                 [else 10])))
          (eval-one-exp '(cond [else 0]))
          (eval-one-exp '(cond [(number? 2)]))
          (eval-one-exp '(or #f #f 3 #f))
          (eval-one-exp '(or #f #f #f))
          (eval-one-exp '(or))
          (eval-one-exp '(let ([x 4] [y 5])
                           (case (+ x y)
                             [(1 3 5 7 9) 'odd]
                             [(0 2 4 6 8) 'even]
                             [else 'out-of-range])))
          (eval-one-exp '(let ([x 4] [y 2])
                           (case (+ x y)
                             [(1 3 5 7 9) 'odd]
                             [(0 2 4 6 8) 'even]
                             [else 'out-of-range])))
          (eval-one-exp '(let ([x 4] [y 6])
                           (case (+ x y)
                             [(1 3 5 7 9) 'odd]
                             [(0 2 4 6 8) 'even]
                             [else 'out-of-range])))
          (eval-one-exp '(case 1 (else 0)))
          (eval-one-exp '(case 1 (() 1) (else 2)))
          (eval-one-exp ' (let ((a (list 5)))
                            (if #t (begin (set-car! a 3)
                                          (set-car! a (+ 3 (car a))) a))))
          (eval-one-exp '(let ([a (list 3)])
                           (while (< (car a) 100000)
                                  (set-car! a (* (car a) (car a)))
                                  (set-car! a (quotient (car a) 2)))
                           a))
          (eval-one-exp '(let ([a (list 3)])
                           (while (< (car a) 3)
                                  (set-car! a (* (car a) (car a)))
                                  (set-car! a (quotient (car a) 2)))
                           a))
          )])
    (display-results correct answers equal?)))

(define (test-literals)
  (let ([correct '(
                   ()
                   #t
                   #f
                   ""
                   "test"
                   #(a b c)
                   #5(a)
                   )]
        [answers
         (list
          (eval-one-exp ''())
          (eval-one-exp #t)
          (eval-one-exp #f)
          (eval-one-exp "")
          (eval-one-exp "test")
          (eval-one-exp ''#(a b c))
          (eval-one-exp ''#5(a))
          )])
    (display-results correct answers equal?)))

(define (test-quote)
  (let ([correct '(
                   ()
                   a
                   (car (a b))
                   (lambda (x) (+ 1 x))
                   )]
        [answers
         (list
          (eval-one-exp '(quote ()))
          (eval-one-exp '(quote a))
          (eval-one-exp '(quote (car (a b))))
          (eval-one-exp '(quote (lambda (x) (+ 1 x))))
          )])
    (display-results correct answers equal?)))

(define (test-if)
  (let ([correct '(
                   5
                   4
                   6
                   2
                   10
                   )]
        [answers
         (list
          (eval-one-exp '(if #t 5 6))
          (eval-one-exp '(if 2 (if #f 3 4) 6))
          (eval-one-exp '(if #f 5 6))
          (eval-one-exp '(if 1 2 3))
          (let ([x (if #f 2 3)])
            (+ x 7))
          )])
    (display-results correct answers equal?)))


(define (test-primitive-procedures-2)
  (let ([correct '(
                   10
                   7
                   48
                   3
                   10
                   #t
                   #f
                   #t
                   (a . b)
                   b
                   (a b c)
                   #t
                   #t
                   #t
                   5
                   #(a b c)
                   #f
                   #t
                   (a b c)
                   #t
                   #t
                   (#t #f)
                   a
                   c
                   b
                   (#t #t #f)
                   )]
        [answers
         (list
          (eval-one-exp '(+ (+ 1 2) 3 4))
          (eval-one-exp '(- 10 1 (- 5 3)))
          (eval-one-exp '(* 2 (* 3 4) 2))
          (eval-one-exp '(/ 6 2))
          (eval-one-exp '(sub1 (add1 10)))
          (eval-one-exp '(not (zero? 3)))
          (eval-one-exp '(= 3 4))
          (eval-one-exp '(>= 4 3))
          (eval-one-exp '(cons 'a 'b))
          (eval-one-exp '(car (cdr '(a b c))))
          (eval-one-exp '(list 'a 'b 'c))
          (eval-one-exp '(null? '()))
          (eval-one-exp '(eq? 'a 'a))
          (eval-one-exp '(equal? 'a 'a))
          (eval-one-exp '(length '(a b c d e)))
          (eval-one-exp '(list->vector '(a b c)))
          (eval-one-exp '(list? 'a))
          (eval-one-exp '(pair? '(a b)))
          (eval-one-exp '(vector->list '#(a b c)))
          (eval-one-exp '(vector? '#(a b c)))
          (eval-one-exp '(number? 5))
          (list (eval-one-exp '(symbol? 'a)) (eval-one-exp '(symbol? 5)))
          (eval-one-exp '(caar '((a b) c)))
          (eval-one-exp '(cadr '((a b) c)))
          (eval-one-exp '(cadar '((a b) c)))
          (eval-one-exp '
           (list (procedure? list)
                 (procedure? (lambda (x y) (list (+ x y))))
                 (procedure? 'list)))
          )])
    (display-results correct answers equal?)))

(define (test-let)
  (let ([correct '(
                   8
                   14
                   24
                   (2 . 4)
                   )]
        [answers
         (list
          (eval-one-exp '
           (let ([a 3][b 5])
             (+ a b)))
          (eval-one-exp '
           (let ([a 3])
             (let ([b 2] [c (+ a 3)] [a (+ a a)])
               (+ a b c))))
          (eval-one-exp '
           (let ([a 3])
             (let ([a (let ([a (+ a a)])
                        (+ a a))])
               (+ a a))))
          (eval-one-exp '
           (let ([a (list 3 4)])
             (set-car! a 2)
             (set-cdr! a (cadr a))
             a))
          )])
    (display-results correct answers equal?)))

(define (test-lambda)
  (let ([correct '(
                   6
                   12
                   154
                   720
                   (#t #t #f)
                   )]
        [answers
         (list
          (eval-one-exp '((lambda (x) (+ 1 x))
                          5))
          (eval-one-exp '((lambda (x) (+ 1 x)
                                  (+ 2 (* 2 x))) 5))
          (eval-one-exp '
           ((lambda (a b)
              (let ([a (+ a b)] [b (- a b)])
                (let ([f (lambda (a) (+ a b))])
                  (f (+ 3 a b)))))
            56 17))
          (eval-one-exp '
           (((lambda (f)
               ((lambda (x)
                  (f (lambda (y)
                       ((x x) y))))
                (lambda (x)
                  (f (lambda (y)
                       ((x x) y))))))
             (lambda (g)
               (lambda (n)
                 (if (zero? n) 1 (* n (g (- n 1)))))))
            6))
          (eval-one-exp '
           (let ([Y (lambda (f)
                      ((lambda (x)
                         (f (lambda (y)
                              ((x x) y))))
                       (lambda (x)
                         (f (lambda (y)
                              ((x x) y))))))]
                 [H (lambda (g)
                      (lambda (x)
                        (if (null? x)
                            '()
                            (cons (procedure? (car x)) (g (cdr x))))))])
             ((Y H) (list list (lambda (x) x) 'list))))
          )])
    (display-results correct answers equal?)))



;;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
    (display ": ")
    (pretty-print
     (if (andmap test-procedure? correct results)
         'All-correct
         `(correct: ,correct yours: ,results)))))

(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)

(define find-edges  ; e know that this node is in the graph before we do the call
  (lambda (graph node)
    (let loop ([graph graph])
      (if (eq? (caar graph) node)
          (cadar graph)
          (loop (cdr graph))))))

;; Problem 8  graph?
(define set?  ;; Is this list a set?  If not, it is not a graph.
  (lambda (list)
    (if (null? list) ;; it's an empty set.
        #t
        (if (member (car list) (cdr list))
            #f
            (set? (cdr list))))))


(define graph?
  (lambda (obj)
    (and (list? obj)
         (let ([syms (map car obj)])
           (and (set? syms)
                (andmap symbol? syms)
                (andmap (lambda (x)
                          (andmap (lambda (y) (member y (remove (car x) syms)))
                                  (cadr x)))
                        obj))))))

(define graph-equal?
  (lambda (a b)
    (and
     (graph? a)
     (graph? b)
     (let ([a-nodes (map car a)]
           [b-nodes (map car b)])
       (and
        (set-equals? a-nodes b-nodes)
                                        ; Now  See if the edges from each node are equivalent in the two graphs.
        (let loop ([a-nodes a-nodes])
          (if (null? a-nodes)
              #t
              (let ([a-edges (find-edges a (car a-nodes))]
                    [b-edges (find-edges b (car a-nodes))])
                (and (set-equals? a-edges b-edges)
                     (loop (cdr a-nodes)))))))))))

(define (test-graph-equal)
  (list
   (graph-equal? '((a (b)) (b (a))) '((b (a)) (a (b))))
   (graph-equal? '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c)))
                 '((b (a c d)) (c (a b d)) (a (b d c)) (d (b a c))))
   (graph-equal? '((a ())) '((a ())))
   (graph-equal? '((a (b c)) (b (a c)) (c (a b))) '((a (b c)) (b (a c)) (c (a b))))
   (graph-equal? '() '())
   ))



(define g test-graph-equal)

;;You can run the tests individually, or run them all
;;#by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'literals)
  (test-literals)
  (display 'quote)
  (test-quote)
  (display 'if)
  (test-if)
  (display 'primitive-procedures-1)
  (test-primitive-procedures-1)
  (display 'let)
  (test-let)
  (display 'lambda)
  (test-lambda)
  (display 'primitive-procedures-2)
  (test-primitive-procedures-2)
  (display 'lambda-regression-tests)
  (test-lambda-regression-tests)
  (display 'lambda-with-variable-args)
  (test-lambda-with-variable-args)
  (display 'syntactic-expansion)
  (test-syntactic-expansion)
  (display 'basics)
  (test-basics)
  (display 'answers-are-sets)
  (test-answers-are-sets)
  (display 'additional)
  (test-additional)
  (display 'subst-leftmost)
  (test-subst-leftmost)
  (display 'set!-local-variables)
  (test-set!-local-variables)
  (display 'simple-defines)
  (test-simple-defines)
  (display 'letrec-and-define)
  (test-letrec-and-define)
  (display 'named-let-and-define)
  (test-named-let-and-define)
  (display 'set!-global-variables)
  (test-set!-global-variables)
  (display 'order-matters!)
  (test-order-matters!)
  (display 'misc)
  (test-misc)
  (display 'ref)
  (test-ref)
  (display 'legacy)
  (test-legacy)
  (display 'simple-call/cc)
  (test-simple-call/cc)
  (display 'complex-call/cc)
  (test-complex-call/cc)
  (display 'exit-list)
  (test-exit-list)

  )

(define r run-all)
