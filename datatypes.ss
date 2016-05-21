
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

(define-datatype continuation continuation?
  (empty-k)
  (begin-k
   (k continuation?)
   (exps (list-of expression?))
   (env environment?))
  (and-k
   (k continuation?)
   (exps (list-of expression?))
   (env environment?))
  (or-k
   (k continuation?)
   (exps (list-of expression?))
   (env environment?))
  (error-k
   (message string?))
  (if-else-k
   (k continuation?)
   (env environment?)
   (true-exp expression?)
   (false-exp expression?))
  (apply-set-ref!-k
   (k continuation?)
   (exp expression?)
   (env environment?))
  (eval-set-ref!-k
   (k continuation?)
   (var ref?))
  (define-k
    (k continuation?)
    (sym symbol?)
    (env environment?))
  (app-exp-k
   (k continuation?)
   (rands (list-of expression?))
   (env environment?))
  (app-proc-rands-k
   (k continuation?)
   (proc-val proc-val?))
  (eval-rands-k
   (k continuation?)
   (env environment?)
   (tail (list-of expression?)))
  (eval-rands-cons-k
   (k continuation?)
   (tail (lambda (x) #t)))
  (form-list-k
   (k continuation?)
   (head (lambda (x) #t)))
  (list-to-cutoff-k
   (k continuation?)
   (args (list-of (lambda (x) #t)))
   (env environment?)
   (body (list-of expression?)))
  (cutoff-to-eval-k
   (k continuation?)
   (env environment?)
   (pars (list-of symbol?))
   (body (list-of expression?)))
  (map-k
   (k continuation?)
   (proc proc-val?)
   (tail (list-of (lambda (x) #t))))
  (map-cons-k
   (k continuation?)
   (head (lambda (x) #t)))
  (deref-k
   (k continuation?))
  (cdr-k
    (k continuation?)))

(define apply-k
  (lambda (k v)
    (cases continuation k
           [empty-k () v]
           [begin-k (k exps env)
                    (if (null? exps)
                        (apply-k k v)
                        (eval-exp (car exps) env (begin-k k (cdr exps) env)))]
           [and-k (k exps env)
                  (if (or (null? exps) (not v))
                      (apply-k k v)
                      (eval-exp (car exps) env (and-k k (cdr exps) env)))]
           [or-k (k exps env)
                 (if (or (null? exps) v)
                     (apply-k k v)
                     (eval-exp (car exps) env (or-k k (cdr exps) env)))]
           [error-k (message)
                    (eopl:error 'apply-env message)]
           [if-else-k (k env t-exp f-exp)
                      (if v
                          (eval-exp t-exp env k)
                          (eval-exp f-exp env k))]
           [apply-set-ref!-k (k exp env)
                             (eval-exp exp env (eval-set-ref!-k k v))]
           [eval-set-ref!-k (k var)
                            (apply-k k (set-ref! var v))]
           [define-k (k sym env)
             (apply-k k (append-env sym (ref v)))]
           [app-exp-k (k rands env)
                      ;;(printf "In app-exp-k with v = ~s\n\trands = ~s\n" v rands)
                      (if (null? rands)
                        (apply-k (app-proc-rands-k k v) '())
                        (eval-exp (car rands)
                                  env
                                  (eval-rands-k (app-proc-rands-k k v)
                                                env
                                                (cdr rands))))]
           [app-proc-rands-k (k proc-value)
                              ;(printf "In app-proc-rands-k with proc-value: ~s\n" proc-value)
                             (apply-proc proc-value v k)]
           [eval-rands-k (k env tail)
                          ;(printf "In eval-rands-k with tail: ~s\n" tail)
                         (if (null? tail)
                             (apply-k k (list v))
                             (eval-exp (car tail) env (eval-rands-k (eval-rands-cons-k k v)
                                                                    env
                                                                    (cdr tail))))]
           [eval-rands-cons-k (k head)
                              (apply-k k (cons head v))]
           [form-list-k (k head)
                        (apply-k k (cons head v))]
           [list-to-cutoff-k (k args env body)
                             (list-cutoff args (- (length v) 1) (cutoff-to-eval-k k env v body))]
           [cutoff-to-eval-k (k env pars body)
                             (eval-exp (car body) (extend-env pars v env) (begin-k k (cdr body) (extend-env pars v env)))]
           [map-k (k proc tail)
                  (if (null? tail)
                    (apply-k k (list v))
                    (apply-proc proc (list (car tail)) (map-k (map-cons-k k v) proc (cdr tail))))]
           [map-cons-k (k head)
                       (apply-k k (cons head v))]
           [deref-k (k)
                    (apply-k k (deref v))]
           [cdr-k (k)
              (apply-k k (cdr v))])))


