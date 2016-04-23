(define syntax-expand
  (lambda (exp)
    (cases expression exp
           [let-exp (vars body)
                    (app-exp (lambda-exp (map 1st vars) (map syntax-expand body))
                             (map syntax-expand (map 2nd vars)))]
           [let*-exp (vars body)
                     (if (null? (cdr vars))
                         (syntax-expand (let-exp vars body))
                         (syntax-expand (let-exp (list (1st vars))
                                                 (list (syntax-expand (let*-exp
                                                                       (cdr vars)
                                                                       body))))))]
           [cond-exp (tests bodies else-body)
                     (if (null? (cdr tests))
                         (if-else-exp (syntax-expand (1st tests))
                                      (begin-exp (map syntax-expand (1st bodies)))
                                      (begin-exp (map syntax-expand else-body)))
                         (if-else-exp (syntax-expand (1st tests))
                                      (begin-exp (map syntax-expand (1st bodies)))
                                      (syntax-expand (cond-exp (cdr tests)
                                                               (cdr bodies)
                                                               else-body))))]
           [else
            exp])))
