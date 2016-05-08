
;; Parsed expression datatypes

(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (lambda-exp
   (pars (list-of pair?))
   (body (list-of expression?)))
  (list-pars-lambda-exp
   (pars symbol?)
   (body (list-of expression?)))
  (improper-pars-lambda-exp
   (pars improper-list-of-symbols?)
   (body (list-of expression?)))
  (app-exp
   (rator expression?)
   (rands (list-of expression?)))
  (set!-exp
   (id symbol?)
   (r-val-exp expression?))
  (let-exp
   (vars (list-of symbol?))
   (args (list-of expression?))
   (body (list-of expression?)))
  (let*-exp
   (vars (list-of symbol?))
   (args (list-of expression?))
   (body (list-of expression?)))
  (named-let-exp
   (id symbol?)
   (vars (list-of symbol?))
   (args (list-of expression?))
   (body (list-of expression?)))
  (letrec-exp
   (vars (list-of symbol?))
   (args (list-of expression?))
   (body (list-of expression?)))
  (if-exp
   (test expression?)
   (true-exp expression?))
  (if-else-exp
   (test expression?)
   (true-exp expression?)
   (false-exp expression?))
  (begin-exp
   (exps (list-of expression?)))
  (and-exp
    (exps (list-of expression?)))
  (or-exp
    (exps (list-of expression?)))
  (cond-exp
   (tests (list-of expression?))
   (bodies (list-of (list-of expression?)))
   (else-body (list-of expression?)))
  (case-exp
    (exp expression?)
    (tests (list-of (list-of expression?)))
    (bodies (list-of (list-of expression?)))
    (else-body (list-of expression?)))
  (while-exp
    (test expression?)
    (body (list-of expression?)))
  (define-exp
    (sym symbol?)
    (val expression?))
  (quote-exp
   (id (lambda (item)
         (and (pair? item)
              (equal? (car item) 'quote)))))
  (lit-exp
   (id (lambda (item)
         (or (number? item)
             (string? item)
             (boolean? item)
             (null? item))))))


;; datatype for procedures.  At first there is only one
;; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (pars (list-of pair?))
   (body (list-of expression?))
   (env environment?)]
  [list-closure
   (pars symbol?)
   (body (list-of expression?))
   (env environment?)]
  [improper-list-closure
   (pars improper-list-of-symbols?)
   (body (list-of expression?))
   (env environment?)])




;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define append-env
  (lambda (env sym val)
    (set-cdr! (2nd env) (map (lambda (x) x) (2nd env)))
    (set-car! (2nd env) sym)

    (set-cdr! (3rd env) (map (lambda (x) x ) (3rd env)))
    (set-car! (3rd env) val)))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))


