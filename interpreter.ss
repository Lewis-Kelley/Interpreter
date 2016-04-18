                                        ; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
                                        ; later we may add things that are not expressions.
    (eval-exp form init-env)))

                                        ; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
           [lit-exp (datum) datum]
           [quote-exp (datum) (2nd datum)]
           [var-exp (id)
                    (apply-env env id; look up its value.
                               (lambda (x) x) ; procedure to call if id is in the environment
                               (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                                                      "variable not found in environment: ~s"
                                                      id)))]
           [if-else-exp (test t-exp f-exp)
                        (if (eval-exp test env)
                            (eval-exp t-exp env)
                            (eval-exp f-exp env))]
           [if-exp (test t-exp)
                   (if (eval-exp test env)
                       (eval-exp t-exp env))]
           [app-exp (rator rands)
                    (let ([proc-value (eval-exp rator env)]
                          [args (eval-rands rands env)])
                      (apply-proc proc-value args))]
           [lambda-exp (pars body)
                       (closure pars body env)]
           [let-exp (vars body)
                    (let ((env (extend-env (map 1st vars) (map (lambda (exp)
                                                                 (eval-exp exp env))
                                                               (map 2nd vars))
                                           env)))
                      (let loop ((ls body))
                        (if (null? (cdr ls))
                            (eval-exp (1st ls) env)
                            (begin
                              (eval-exp (1st ls) env)
                              (loop (cdr ls))))))]
           [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

                                        ; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

                                        ;  Apply a procedure to its arguments.
                                        ;  At this point, we only have primitive procedures.
                                        ;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
           [prim-proc (op) (apply-prim-proc op args)]
                                        ; You will add other cases
           [closure (pars body env)
                    (let ((env (extend-env pars args env)))
                      (let loop ((ls body))
                        (if (not (null? (cdr ls)))
                            (begin (eval-exp (car ls) env) (loop (cdr ls)))
                            (eval-exp (car ls) env))
                        ))]
           
           [else (error 'apply-proc
                        "Attempt to apply bad procedure: ~s"
                        proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < > <= >= cons car
                              cdr list null? assq eq? equal? atom? length
                              list->vector list? pair? procedure? vector->list
                              vector make-vector vector-ref vector? number? symbol?
                              set-car! set-cdr! vector-set! display newline
                              caar cadr cdar cddr caaar caadr cadar cdaar
                              caddr cdadr cddar cdddr exit))

(define init-env         ; for now, our initial global environment only contains
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier.
   (map prim-proc
        *prim-proc-names*)
   (empty-env)))

                                        ; Usually an interpreter must define each
                                        ; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc ;;TODO Complete for all the listed prim-procs above
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-)
       (if (null? args)
           (eopl:error 'apply-prim-proc "Invalid arguments to -: ~s" args)
           (apply - args))]
      [(*) (apply * args)]
      [(/)
       (if (null? args)
           (eopl:error 'apply-prim-proc "Invalid arguments to /: ~s" args)
           (apply / args))]
      [(add1)
       (if (or (null? args) (not (null? (cdr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to add1: ~s" args)
           (+ (1st args) 1))]
      [(sub1)
       (if (or (null? args) (not (null? (cdr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to sub1: ~s" args)
           (- (1st args) 1))]
      [(zero?)
       (if (or (null? args) (not (null? (cdr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to zero: ~s" args)
           (zero? (1st args)))]
      [(not)
       (if (or (null? args) (not (null? (cdr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to not: ~s" args)
           (not (1st args)))]
      [(=)
       (if (null? args)
           (eopl:error 'apply-prim-proc "Invalid arguments to =: ~s" args)
           (apply = args))]
      [(<)
       (if (null? args)
           (eopl:error 'apply-prim-proc "Invalid arguments to <: ~s" args)
           (apply < args))]
      [(<=)
       (if (null? args)
           (eopl:error 'apply-prim-proc "Invalid arguments to <=: ~s" args)
           (apply <= args))]
      [(>)
       (if (null? args)
           (eopl:error 'apply-prim-proc "Invalid arguments to >: ~s" args)
           (apply > args))]
      [(>=)
       (if (null? args)
           (eopl:error 'apply-prim-proc "Invalid arguments to >=: ~s" args)
           (apply >= args))]
      [(cons)
       (if (or (null? args) (null? (cdr args)) (not (null? (cddr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to cons: ~s" args)
           (cons (1st args) (2nd args)))]
      [(car)
       (if (or (null? args) (not (null? (cdr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to car: ~s" args)
           (car args))]
      [(cdr)
       (if (or (null? args) (not (null? (cdr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to cdr: ~s" args)
           (cdr args))]
      [(list) (apply list args)]
      [(null?)
       (if (or (null? args) (not (null? (cdr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to null: ~s" args)
           (null? (1st args)))]
      [(assq)
       (if (or (null? args) (null? (cdr args)) (null? (cddr args)) (not (null? (cdddr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to assq: ~s" args)
           (assq (1st args) (2nd args)))]
      [(eq?)
       (if (or (null? args) (null? (cdr args)) (null? (cddr args)) (not (null? (cdddr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to eq?: ~s" args)
           (eq? (1st args) (2nd args)))]
      [(equal?)
       (if (or (null? args) (null? (cdr args)) (null? (cddr args)) (not (null? (cdddr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to equal?: ~s" args)
           (equal? (1st args) (2nd args)))]
      [(atom?)
       (if (or (null? args) (not (null? (cdr args))))
           (eopl:error 'apply-prim-proc "Invalid arguments to atom?: ~s" args)
           (atom? (1st args)))]
      [else (eopl:error 'apply-prim-proc
                   "Bad primitive procedure name: ~s"
                   prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))
