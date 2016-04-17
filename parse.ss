; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
  (lambda (datum)
    (cond
                                        ; var-exp
     ((symbol? datum)
      (var-exp datum))

                                        ; lit-exp
     ((or (number? datum) (vector? datum) (string? datum) (null? datum)
          (boolean? datum))
      (lit-exp datum))
     ((pair? datum)
      (cond

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
                (not (null? (cddddr datum))))
            (eopl:error 'parse-exp "*if-exp* incorrect length ~s" datum))

        (if (null? (cdddr datum))
            (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
            (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))))
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
                             (map (op-on-2nd parse-exp) (3rd datum))
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

              (let-exp (map (op-on-2nd parse-exp) (2nd datum))
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

        (let*-exp (map (op-on-2nd parse-exp) (2nd datum))
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

        (letrec-exp (map (op-on-2nd parse-exp) (2nd datum))
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
           (lit-exp (id)
                    id))))
