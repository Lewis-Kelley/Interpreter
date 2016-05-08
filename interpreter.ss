;; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    (eval-exp form init-env)))

(define reset-global-env (lambda () (set! init-env
                                      (extend-env            ; procedure names.  Recall that an environment associates
                                       *prim-proc-names*   ;  a value (not an expression) with an identifier.
                                       (map prim-proc
                                            *prim-proc-names*)
                                       (empty-env)))))

;; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
           [lit-exp (datum) datum]
           [quote-exp (datum) (2nd datum)]
           [begin-exp (exps)
                      (for-each (lambda (exp) (eval-exp exp env)) exps)]
           [and-exp (exps)
                    (if (null? exps)
                        #t
                        (let loop ((exps exps))
                          (if (null? (cdr exps))
                              (eval-exp (car exps) env)
                              (if (eval-exp (car exps) env)
                                  (loop (cdr exps))
                                  #f))))]
           [or-exp (exps)
                   (if (null? exps)
                       #f
                       (let loop ((exps exps))

                         (if (null? (cdr exps))
                             (eval-exp (car exps) env)
                             (let ((res (eval-exp (car exps) env)))
                               (if res
                                   res
                                   (loop (cdr exps)))))))]
           [while-exp (test bodies)
                      (let loop ()
                        (if (eval-exp test env)
                            (begin (for-each (lambda (exp) (eval-exp exp env)) bodies)
                                   (loop))))]
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
                       (eval-exp t-exp env)
                       (void))]
           [app-exp (rator rands)
                    (let ([proc-value (eval-exp rator env)]
                          [args rands])
                      (apply-proc proc-value args env))]
           [lambda-exp (pars body)
                       (closure pars body env)]
           [list-pars-lambda-exp (pars body)
                                 (list-closure pars body env)]
           [improper-pars-lambda-exp (pars body)
                                     (improper-list-closure pars body env)]
           [set!-exp (id exp)
                     (set-ref!
                      (apply-env-ref env id (lambda (x) x) (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                                                                                  "variable not found in environment: ~s"
                                                                                  id)))
                      (eval-exp exp env))]
           [define-exp (sym val)
             (append-env env sym (box (eval-exp val env)))]
           [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;; Converts an improper list to a proper list.
;; WARNING: Does not check if the argument is actually improper.
;;          This _will_ break!
(define i-list->list
  (lambda (i-list)
    (if (pair? (cdr i-list))
        (cons (car i-list) (i-list->list (cdr i-list)))
        (list (car i-list) (cdr i-list)))))

;; Takes a list and a target length. Returns the contents of
;; the list in a shortened list where there are len + 1 elements
;; with the last being a list of the leftovers.
(define list-cutoff
  (lambda (ls len)
    (if (equal? len 1)
        (list (car ls) (cdr ls))
        (cons (car ls) (list-cutoff (cdr ls) (- len 1))))))

(define apply-proc
  (lambda (proc-value args cur-env)
    (cases proc-val proc-value
           [prim-proc (op) (apply-prim-proc op args cur-env)]
           [closure (pars body env)
                    (let ((env (extended-env-record
                                (map car pars)
                                (let build-args ((pars pars) (args args))
                                  (if (null? pars)
                                      '()
                                      (if (cdr (1st pars))
                                          (cons (apply-env-ref cur-env
                                                               (1st args)
                                                               (lambda (x) x)
                                                               (lambda ()
                                                                 (eopl:error 'apply-proc
                                                                             "Couldn't find ~s\n"
                                                                             (1st args))))
                                                (build-args (cdr pars) (cdr args)))
                                          (cons (box (eval-exp (1st args) cur-env))
                                                (build-args (cdr pars) (cdr args))))))
                                env)))
                      (let loop ((ls body))
                        (if (not (null? (cdr ls)))
                            (begin (eval-exp (car ls) env) (loop (cdr ls)))
                            (eval-exp (car ls) env))))]
           [list-closure (pars body env)
                         (let ((env (extend-env (list pars) (list (map (lambda (x) (eval-exp x cur-env)) args)) env)))
                           (let loop ((ls body))
                             (if (not (null? (cdr ls)))
                                 (begin (eval-exp (car ls) env) (loop (cdr ls)))
                                 (eval-exp (car ls) env))))]
           [improper-list-closure (pars body env)
                                  (let ((pars (i-list->list pars)))
                                    (let ((env (extend-env pars (list-cutoff (map (lambda (x) (eval-exp x cur-env)) args) (- (length pars) 1)) env)))
                                      (let loop ((ls body))
                                        (if (not (null? (cdr ls)))
                                            (begin (eval-exp (car ls) env) (loop (cdr ls)))
                                            (eval-exp (car ls) env)))))]
           [else (error 'apply-proc
                        "Attempt to apply bad procedure: ~s"
                        proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < > <= >= cons car
                              cdr list null? assq eq? equal? atom? length
                              list->vector list? pair? procedure? vector->list
                              vector make-vector vector-ref vector? number? symbol?
                              set-car! set-cdr! vector-set! display newline
                              caar cadr cdar cddr caaar caadr cadar cdaar
                              caddr cdadr cddar cdddr map apply member quotient
                              eqv? append list-tail void))

(define init-env         ; for now, our initial global environment only contains
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier.
   (map prim-proc
        *prim-proc-names*)
   (empty-env)))

(define arg-test
  (lambda (pred?)
    (lambda (sym args cur-env)
      (if (not (pred? args))
          (eopl:error 'apply-prim-proc "Invalid arguments to ~s: ~s" sym args)
          (apply (eval sym) (map (lambda (x) (eval-exp x cur-env)) args))))))

(define zero-arg
  (arg-test (lambda (args) (null? args))))

(define one-arg
  (arg-test (lambda (args) (and (not (null? args)) (null? (cdr args))))))

(define two-arg
  (arg-test (lambda (args) (and (not (null? args)) (not (null? (cdr args))) (null? (cddr args))))))

(define three-arg
  (arg-test (lambda (args) (and (not (null? args)) (not (null? (cdr args))) (not (null? (cddr args)))
                                (null? (cdddr args))))))

(define one-two-arg
  (arg-test (lambda (args) (and (not (null? args)) (or (null? (cdr args)) (null? (cddr args)))))))

(define non-zero-arg
  (arg-test (lambda (args) (not (null? args)))))

(define zero-one-arg
  (arg-test (lambda (args) (or (null? args) (null? (cdr args))))))

(define any-arg (arg-test (lambda (args) #t)))

(define apply-prim-proc
  (lambda (prim-proc args cur-env)
    ((case prim-proc
       [(+) any-arg]
       [(-) non-zero-arg]
       [(*) any-arg]
       [(/) non-zero-arg]
       [(add1) one-arg]
       [(sub1) one-arg]
       [(zero?) one-arg]
       [(not) one-arg]
       [(=) non-zero-arg]
       [(<) non-zero-arg]
       [(<=) non-zero-arg]
       [(>) non-zero-arg]
       [(>=) non-zero-arg]
       [(cons) two-arg]
       [(car) one-arg]
       [(cdr) one-arg]
       [(list) any-arg]
       [(null?) one-arg]
       [(assq) two-arg]
       [(eq?) two-arg]
       [(equal?) two-arg]
       [(atom?) one-arg]
       [(length) one-arg]
       [(list->vector) one-arg]
       [(list?) one-arg]
       [(pair?) one-arg]
       [(procedure?) (lambda (prim-proc args cur-env) ;; Effectively shadow procedure? with our proc-val?
                       (if (or (null? args) (not (null? (cdr args))))
                           (eopl:error "Invalid arguments to ~s: ~s" prim-proc args)
                           (apply proc-val? (eval-exp (1st args) cur-env))))]
       [(vector->list) one-arg]
       [(vector) any-arg]
       [(make-vector) one-two-arg]
       [(vector-ref) two-arg]
       [(vector?) one-arg]
       [(number?) one-arg]
       [(symbol?) one-arg]
       [(set-car!) two-arg]
       [(set-cdr!) two-arg]
       [(vector-set!) three-arg]
       [(display) one-two-arg]
       [(newline) zero-one-arg]
       [(caar) one-arg]
       [(cadr) one-arg]
       [(cdar) one-arg]
       [(cddr) one-arg]
       [(caaar) one-arg]
       [(caadr) one-arg]
       [(cadar) one-arg]
       [(cdaar) one-arg]
       [(caddr) one-arg]
       [(cdadr) one-arg]
       [(caddr) one-arg]
       [(cdddr) one-arg]
       [(map) (lambda (prim-proc args cur-env)
                (if (or (null? args) (null? (cdr args)) (not (list? (2nd args))) (not (null? (cddr args))))
                    (eopl:error "Invalid arguments to ~s: ~s" prim-proc args)
                    (let loop ((args (2nd args))
                               (proc (1st args)))
                      (if (null? args)
                          '()
                          (cons (apply-proc (eval-exp proc cur-env) (list (car args)) cur-env) (loop (cdr args) proc))))))]
       [(apply) (lambda (prim-proc args cur-env)
                  (if (or (null? args) (not (proc-val? (1st args))) (null? (cdr args)) (not (list? (2nd args))) (not (null? (cddr args))))
                      (eopl:error "Invalid arguments to ~s: ~s" prim-proc args)
                      (apply-proc (eval-exp (1st args) cur-env) (2nd args) cur-env)))]
       [(member) two-arg]
       [(quotient) two-arg]
       [(eqv?) two-arg]
       [(append) any-arg]
       [(list-tail) two-arg]
       [(void) zero-arg]
       [else (eopl:error 'apply-prim-proc
                         "Bad primitive procedure name: ~s"
                         prim-proc)]) prim-proc args cur-env)))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
