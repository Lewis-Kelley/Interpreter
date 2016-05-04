(define syntax-expand
  (lambda (exp)
    (cases expression exp
           [let-exp (vars args body)
                    (app-exp (lambda-exp vars (map syntax-expand body))
                             (map syntax-expand args))]
           [let*-exp (vars args body)
                     (if (null? (cdr vars))
                         (syntax-expand (let-exp vars args body))
                         (syntax-expand (let-exp (list (1st vars))
                                                 (list (1st args))
                                                 (list (syntax-expand (let*-exp
                                                                       (cdr vars)
                                                                       body))))))]
           [cond-exp (tests bodies else-body)
                     (cond
                      ((null? tests)
                       (begin-exp (map syntax-expand else-body)))
                      ((null? (cdr tests))
                       (if-else-exp (syntax-expand (1st tests))
                                    (if (null? (1st bodies))
                                        (lit-exp '#t)
                                        (begin-exp (map syntax-expand (1st bodies))))
                                    (begin-exp (map syntax-expand else-body))))
                      (else (if-else-exp (syntax-expand (1st tests))
                                         (if (null? (1st bodies))
                                             (lit-exp '#t)
                                             (begin-exp (map syntax-expand (1st bodies))))
                                         (syntax-expand (cond-exp (cdr tests)
                                                                  (cdr bodies)
                                                                  else-body)))))]
           [case-exp (exp tests bodies else-body)
                     (syntax-expand (cond-exp
                                     (map (lambda (x)
                                            (app-exp (var-exp 'member)
                                                     (list (syntax-expand exp)
                                                           (app-exp (var-exp 'list)
                                                                    (map syntax-expand x)))))
                                          tests)
                                     bodies
                                     else-body))]
           [lit-exp (datum)
                    (lit-exp datum)]
           [quote-exp (datum)
                      (quote-exp datum)]
           [begin-exp (exps)
                      (begin-exp (map syntax-expand exps))]
           [and-exp (exps)
                    (and-exp (map syntax-expand exps))]
           [or-exp (exps)
                   (or-exp (map syntax-expand exps))]
           [var-exp (id)
                    (var-exp id)]
           [if-else-exp (test t-exp f-exp)
                        (if-else-exp (syntax-expand test)
                                     (syntax-expand t-exp)
                                     (syntax-expand f-exp))]
           [if-exp (test t-exp)
                   (if-exp (syntax-expand test)
                           (syntax-expand t-exp))]
           [app-exp (rator rands)
                    (app-exp (syntax-expand rator)
                             (map syntax-expand rands))]
           [lambda-exp (pars body)
                       (lambda-exp pars
                                   (map syntax-expand body))]
           [list-pars-lambda-exp (pars body)
                                 (list-pars-lambda-exp pars
                                                       (map syntax-expand body))]
           [improper-pars-lambda-exp (pars body)
                                     (improper-pars-lambda-exp pars
                                                               (map syntax-expand body))]
            [while-exp (test bodies)
              (while-exp 
                (syntax-expand test)
                (map syntax-expand bodies))]
            [set!-exp (id exp)
              (set!-exp
                id
                (syntax-expand exp))]
           [else
            (eopl:error 'syntax-expand "Unrecognized expression: ~s" exp)])))
