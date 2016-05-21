                                        ; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define adv-extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env
  (lambda (syms vals env)
    (adv-extend-env syms (map box vals) env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
             (if (number? list-index-r)
                 (+ 1 list-index-r)
                 #f))))))

(define ref box)

(define ref? box?)

(define deref unbox)

(define set-ref! set-box!)

(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    ;(printf "in apply-env-ref with sym ~s and env: ~s\n" sym env)
    (cases environment env
           (empty-env-record ()
                             (apply-k fail '()))
           (extended-env-record (syms vals env)
                                (let ((pos (list-find-position sym syms)))
                                  (if (number? pos)
                                      (apply-k succeed (list-ref vals pos))
                                      (apply-env-ref env sym succeed fail)))))))

(define apply-env

  (lambda (env sym succeed fail)
    ;(printf "in apply-env\n")
    (apply-env-ref env sym (deref-k succeed) fail)))
