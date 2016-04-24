                                        ; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

                                        ; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

                                        ; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

;; Check to see if the given pair is a valid variable definition
(define var-def-pair?
  (lambda (pair)
    (and (symbol? (1st pair))
         (pair? (cdr pair))
         (expression? (parse-exp (2nd pair)))
         (null? (cddr pair)))))

;; Check if the list is an improper list
(define improper-list?
  (lambda (ls)
    (and (pair? ls)
         (cond
          ((null? (cdr ls))
           #f)
          ((not (pair? (cdr ls)))
           #t)
          (else
           (improper-list? (cdr ls)))))))

(define improper-list-of-symbols?
  (lambda (ls)
    (cond
     ((symbol? ls)
      #t)
     ((pair? ls)
      (and (symbol? (car ls))
           (improper-list-of-symbols? (cdr ls))))
     (else
      #f))))

;; Returns a function that executes the function only on the second element in a pair
;; (for use with let expressions)
(define op-on-2nd
  (lambda (func)
    (lambda (pair)
      (list (1st pair)
            (func (2nd pair))))))

(define parse-exp
  (lambda (datum)
    (cond
                                        ; var-exp
     ((symbol? datum)
      (var-exp datum))

                                        ; lit-exp
     ((or (number? datum) (string? datum) (null? datum)
          (boolean? datum))
      (lit-exp datum))
     ((pair? datum)
      (cond
                                        ; quote-exp
       ((equal? (car datum) 'quote)
        (quote-exp datum))
                                        ; begin-exp
       ((equal? (car datum) 'begin)
        (begin-exp (map parse-exp (cdr datum))))
                                        ; and-exp
       ((equal? (car datum) 'and)
        (and-exp (map parse-exp (cdr datum))))
                                        ; or-exp
       ((equal? (car datum) 'or)
        (or-exp (map parse-exp (cdr datum))))
                                        ; cond-exp
       ((equal? (car datum) 'cond)
        (if (null? (cdr datum))
            (eopl:error 'parse-exp "*cond-exp* incorrect length ~s" datum))
        (let ((tests (map 1st (cdr datum))) (bodies (map cdr (cdr datum))))
          (let ((rev-tests (reverse tests)))
            (if (equal? (car rev-tests) 'else)
                (let ((rev-bodies (reverse bodies)))
                  (if (null? (car rev-bodies))
                      (eopl:error 'parse-exp "*cond-exp* incorrect length ~s" datum)
                      (cond-exp (map parse-exp (reverse (cdr rev-tests)))
                                (map (lambda (body)
                                       (map parse-exp body))
                                     (reverse (cdr rev-bodies)))
                                (map parse-exp (car rev-bodies)))))
                (cond-exp (map parse-exp tests)
                          (map (lambda (body)
                                 (map parse-exp body))
                               bodies)
                          '())))))
                                        ; case-exp
       ((equal? (car datum) 'case)
        (let ((tests (map 1st (cddr datum))) (bodies (map cdr (cddr datum))))
          (for-each (lambda (body)
                      (if (null? body)
                          (eopl:error 'parse-exp "*case-exp* incorrect length ~s" datum)))
                    bodies)
          (if (equal? (car (reverse tests)) 'else)
              (let ((rev-bodies (reverse bodies)))
                (case-exp (parse-exp (2nd datum)) (map (lambda (x) (map parse-exp x)) (reverse (cdr (reverse tests))))
                          (map (lambda (body)
                                 (map parse-exp body))
                               (reverse (cdr rev-bodies)))
                          (map parse-exp (car rev-bodies))))
              (case-exp (parse-exp (2nd datum)) (map (lambda (x) (map parse-exp x)) tests)
                        (map (lambda (body)
                               (map parse-exp body))
                             bodies)
                        '()))))
                                        ; set!-exp
       ((equal? (1st datum) 'set!)

        (if (or (null? (cdr datum))
                (null? (cddr datum))
                (not (null? (cdddr datum))))
            (eopl:error 'parse-exp "*set!-exp* incorrect length ~s" datum))

        (set!-exp (2nd datum) (parse-exp (3rd datum))))

                                        ; if-exp/if-else-exp
       ((equal? (1st datum) 'if)

        (if (or (null? (cdr datum))
                (null? (cddr datum))
                )
            (eopl:error 'parse-exp "*if-exp* incorrect length ~s" datum))

        (if (null? (cdddr datum))
            (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
            (if (not (null? (cddddr datum)))
                (eopl:error 'parse-exp "*if-exp* incorrect length ~s" datum)
                (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum))))))
                                        ;
                                        ; lambda's
                                        ;
       ((equal? (1st datum) 'lambda)

        (if (or (null? (cdr datum))
                (null? (cddr datum)))
            (eopl:error 'parse-exp "*lambda-exp* incorrect length ~s" datum))

        (cond

                                        ; list-pars-lambda-exp
         ((symbol? (2nd datum))
          (list-pars-lambda-exp (2nd datum) (map parse-exp (cddr datum))))

                                        ; improper-pars-lambda-exp
         ((improper-list? (2nd datum))

          (if (not (improper-list-of-symbols? (2nd datum)))
              (eopl:error 'parse-exp "*improper-pars-lambda-exp* parameters must be symbols ~s" datum))

          (improper-pars-lambda-exp (2nd datum) (map parse-exp (cddr datum))))
         (else

                                        ; lambda-exp
          (if (not ((list-of symbol?) (2nd datum)))
              (eopl:error 'parse-exp "*lambda-exp* parameters must be symbols ~s" datum))

          (lambda-exp (2nd datum) (map parse-exp (cddr datum))))))
                                        ;
                                        ; let's
                                        ;
       ((equal? (1st datum) 'let)
        (if (symbol? (2nd datum))
            (begin
                                        ; named-let-exp
              (if (not (or (pair? (3rd datum)) (null? (3rd datum))))
                  (eopl:error 'parse-exp "*named-let-exp* no variable definitions given ~s" datum))
              (if (not ((list-of var-def-pair?) (3rd datum)))
                  (eopl:error 'parse-exp "*named-let-exp* variable definitions must be a list of pairs ~s" datum))
              (if (not ((list-of symbol?) (map car (3rd datum))))
                  (eopl:error 'parse-exp "*named-let-exp* must use symbols to define variables ~s" datum))
              (if (null? (cdddr datum))
                  (eopl:error 'parse-exp "*named-let-exp* must have a body ~s" datum))

              (named-let-exp (2nd datum)
                             (map 1st (3rd datum))
                             (map (lambda (item)
                                    (parse-exp (2nd item)))
                                  (3rd datum))
                             (map parse-exp (cdddr datum))))

                                        ; let-exp
            (begin
              (if (not (or (pair? (2nd datum)) (null? (2nd datum))))
                  (eopl:error 'parse-exp "*let-exp* no variable definitions given ~s" datum))
              (if (not ((list-of var-def-pair?) (2nd datum)))
                  (eopl:error 'parse-exp "*let-exp* variable definitions must be a list of pairs ~s" datum))
              (if (not ((list-of symbol?) (map car (2nd datum))))
                  (eopl:error 'parse-exp "*let-exp* must use symbols to define variables ~s" datum))
              (if (null? (cddr datum))
                  (eopl:error 'parse-exp "*let-exp* must have a body ~s" datum))

              (let-exp (map 1st (2nd datum))
                       (map (lambda (item)
                              (parse-exp (2nd item)))
                            (2nd datum))
                       (map parse-exp (cddr datum))))))

                                        ; let*-exp
       ((equal? (1st datum) 'let*)

        (if (not (or (pair? (2nd datum)) (null? (2nd datum))))
            (eopl:error 'parse-exp "*let*-exp* no variable definitions given ~s" datum))
        (if (not ((list-of var-def-pair?) (2nd datum)))
            (eopl:error 'parse-exp "*let*-exp* variable definitions must be a list of pairs ~s" datum))
        (if (not ((list-of symbol?) (map car (2nd datum))))
            (eopl:error 'parse-exp "*let*-exp* must use symbols to define variables ~s" datum))
        (if (null? (cddr datum))
            (eopl:error 'parse-exp "*let*-exp* must have a body ~s" datum))

        (let*-exp (map 1st (2nd datum))
                  (map (lambda (item)
                         (parse-exp (2nd item)))
                       (2nd datum))
                  (map parse-exp (cddr datum))))

                                        ; letrec-exp
       ((equal? (1st datum) 'letrec)

        (if (not (or (pair? (2nd datum)) (null? (2nd datum))))
            (eopl:error 'parse-exp "*letrec-exp* no variable definitions given ~s" datum))
        (if (not ((list-of var-def-pair?) (2nd datum)))
            (eopl:error 'parse-exp "*letrec-exp* variable definitions must be a list of pairs ~s" datum))
        (if (not ((list-of symbol?) (map car (2nd datum))))
            (eopl:error 'parse-exp "*letrec-exp* must use symbols to define variables ~s" datum))
        (if (null? (cddr datum))
            (eopl:error 'parse-exp "*letrec-exp* must have a body ~s" datum))

        (letrec-exp (map 1st (2nd datum))
                    (map (lambda (item)
                           (parse-exp (2nd item)))
                         (2nd datum))
                    (map parse-exp (cddr datum))))

                                        ; app-exp
       (else
        (if (improper-list? datum)
            (eopl:error 'parse-exp "*app-exp* not a proper list ~s" datum))
        (app-exp (parse-exp (1st datum))
                 (map parse-exp (cdr datum))))))
     (else (eopl:error 'parse-exp "bad expression: ~s" datum)))))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
           (var-exp (id)
                    id)
           (lambda-exp (id body)
                       (cons 'lambda
                             (cons id
                                   (map unparse-exp body))))
           (list-pars-lambda-exp (pars body)
                                 (cons 'lambda
                                       (cons pars
                                             (map unparse-exp body))))
           (improper-pars-lambda-exp (pars body)
                                     (append (list 'lambda
                                                   pars)
                                             (map unparse-exp body)))
           (app-exp (rator rands)
                    (cons (unparse-exp rator)
                          (map unparse-exp rands)))
           (set!-exp (id r-val-exp)
                     (list 'set! id (unparse-exp r-val-exp)))
           (let-exp (vars body)
                    (cons 'let
                          (cons (map (op-on-2nd unparse-exp) vars)
                                (map unparse-exp body))))
           (let*-exp (vars body)
                     (cons 'let*
                           (cons (map (op-on-2nd unparse-exp) vars)
                                 (map unparse-exp body))))
           (named-let-exp (id pars body)
                          (cons 'letrec
                                (cons id
                                      (cons (op-on-2nd unparse-exp pars)
                                            (map unparse-exp body)))))
           (letrec-exp (vars body)
                       (cons 'letrec
                             (cons (map (op-on-2nd unparse-exp) vars)
                                   (map unparse-exp body))))
           (if-exp (test true-exp)
                   (list 'if
                         (unparse-exp test)
                         (unparse-exp true-exp)))
           (if-else-exp (test true-exp false-exp)
                        (list 'if
                              (unparse-exp test)
                              (unparse-exp true-exp)
                              (unparse-exp false-exp)))
           (quote-exp (id)
                      (cdr id))
           (lit-exp (id)
                    id))))
