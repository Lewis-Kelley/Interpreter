;; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    (eval-exp form init-env (empty-k))))

(define reset-global-env (lambda () (set! init-env
                                      (extend-env            ; procedure names.  Recall that an environment associates
                                       *prim-proc-names*   ;  a value (not an expression) with an identifier.
                                       (map prim-proc
                                            *prim-proc-names*)
                                       (empty-env)))))

;; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env k)
    ;(printf "In eval-exp with exp ~s\n" exp)
    (cases expression exp
           [lit-exp (datum) (apply-k k datum)]
           [quote-exp (datum) (apply-k k (2nd datum))]
           [begin-exp (exps)
                      (eval-exp (car exps) env (begin-k k (cdr exps) env))]
           [and-exp (exps)
                    (if (null? exps)
                        (apply-k k #t)
                        (eval-exp (car exps) env (and-k k (cdr exps) env)))]
           [or-exp (exps)
                   (if (null? exps)
                       (apply-k k #f)
                       (eval-exp (car exps) env (or-k k (cdr exps) env)))]
           [var-exp (id)
                    ;(printf "in var-exp with id: ~s\n" id)
                    (apply-env env id; look up its value.
                               k ; procedure to call if id is in the environment
                               (error-k (format "Failed to lookup ~s" id)))]
           [if-else-exp (test t-exp f-exp)
                        (eval-exp test env (if-else-k k env t-exp f-exp))]
           [if-exp (test t-exp)
                   (eval-exp test env (if-else-k k env t-exp (app-exp (var-exp void) '())))]
           [app-exp (rator rands)
                    ;(printf "In app-exp with rator ~s\n" rator)
                    (eval-exp rator env (app-exp-k k rands env))]
           [lambda-exp (pars body)
                       (apply-k k (closure pars body env))]
           [list-pars-lambda-exp (pars body)
                                 (apply-k k (list-closure pars body env))]
           [improper-pars-lambda-exp (pars body)
                                     (apply-k k (improper-list-closure pars body env))]
           [set!-exp (id exp)
                     (apply-env-ref env id (apply-set-ref!-k k exp env) (error-k (format "Failed to lookup ~s" id)))]
           [define-exp (sym val)
             (append-env env sym (box (eval-exp val env)))]
           [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
        (apply-k k '())
        (eval-rands (cdr rands) env (eval-rands-k k (car rands))))))

;; Converts an improper list to a proper list.
;; WARNING: Does not check if the argument is actually improper.
;;          This _will_ break!
(define i-list->list
  (lambda (i-list k)
    (if (pair? (cdr i-list))
        (i-list->list (cdr i-list) (form-list-k k (car i-list)))
        (apply-k k (cons (car i-list) (list (cdr i-list)))))))

;; Takes a list and a target length. Returns the contents of
;; the list in a shortened list where there are len + 1 elements
;; with the last being a list of the leftovers.
(define list-cutoff
  (lambda (ls len k)
    (if (equal? len 1)
        (apply-k k (list (car ls) (cdr ls)))
        (list-cutoff (cdr ls) (- len 1) (form-list-k k (car ls))))))

(define apply-proc
  (lambda (proc-value args k)
    ;(printf "In apply-proc with args ~s\n" args)
    ;(printf "Apply-proc continuation ~s\n" k)
    (cases proc-val proc-value
           [prim-proc (op) (apply-prim-proc op args k)]
           [closure (pars body env)
                    ;(printf "In closure with pars: ~s,\n body: ~s,\n and env: ~s\n" pars body env)
                    (eval-exp (car body) (extend-env (map car pars) args env) (begin-k k (cdr body) (extend-env (map car pars) args env)))]
           [list-closure (pars body env)
                         (eval-exp (car body) (extend-env (list pars) (list args) env) (begin-k k (cdr body) (extend-env (list pars) (list args) env)))]
           [improper-list-closure (pars body env)
                          (i-list->list pars (list-to-cutoff-k k args env body))]
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
                              eqv? append list-tail void display newline))

(define init-env         ; for now, our initial global environment only contains
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier.
   (map prim-proc
        *prim-proc-names*)
   (empty-env)))

(define arg-test
  (lambda (pred?)
    (lambda (sym args k)
      ;(printf "In ~s with args ~s\n" sym args)
      (if (not (pred? args))
          (eopl:error 'apply-prim-proc "Invalid arguments to ~s: ~s" sym args)
          (apply-k k (apply (eval sym) args))))))

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
  (lambda (prim-proc args k)
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
       [(procedure?) (lambda (prim-proc args k) ;; Effectively shadow procedure? with our proc-val?
                       (if (or (null? args) (not (null? (cdr args))))
                           (eopl:error 'apply-prim-proc "Invalid arguments to ~s: ~s" prim-proc args)
                           (apply-k k (apply proc-val? args))))]
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
       [(map) (lambda (prim-proc args k)
                (if (or (null? args) (not (proc-val? (1st args))) (null? (cdr args))
                        (not (list? (2nd args))) (not (null? (cddr args))))
                    (eopl:error "Invalid arguments to ~s: ~s" prim-proc args)
                    (apply-proc (1st args) 
                                (list (1st (2nd args))) 
                                (map-k k
                                       (1st args) 
                                       (cdr (2nd args))))))]
                    ;(prim-proc-map-cps (1st args) (cdr args) k)))]
       [(apply) (lambda (prim-proc args k)
                  (if (or (null? args) (not (proc-val? (1st args))) (null? (cdr args)) (not (list? (2nd args))) (not (null? (cddr args))))
                      (eopl:error "Invalid arguments to ~s: ~s" prim-proc args)
                      (apply-proc (1st args) (2nd args) k)))]
       [(member) two-arg]
       [(quotient) two-arg]
       [(eqv?) two-arg]
       [(append) any-arg]
       [(list-tail) two-arg]
       [(void) zero-arg]
       [(display) one-arg]
       [(newline) zero-arg]
       [else (eopl:error 'apply-prim-proc
                         "Bad primitive procedure name: ~s"
                         prim-proc)]) prim-proc args k)))

(define prim-proc-map-cps
  (lambda (proc args k)
    (if (null? args)
        (apply-k k '())
        (prim-proc-map-cps proc (cdr args) (map-k k proc (1st args))))))


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
