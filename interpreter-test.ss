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
		(list (odd? 3) (even? 3) (odd? 4) (even? 4))))	     )])
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

(define (test-primitive-procedures)
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


(define (test-primitive-procedures)
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
  (display 'primitive-procedures) 
  (test-primitive-procedures)    
  (display 'let) 
  (test-let)
  (display 'lambda) 
  (test-lambda)
  (display 'primitive-procedures) 
  (test-primitive-procedures)
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
  )

(define r run-all)

