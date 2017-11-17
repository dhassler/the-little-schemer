(define eq?-c
  (lambda (a)
    (lambda (x) (eq? a x))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (old new lat)
      (cond
        ((null? lat) '())
        ((test? old (car lat)) (cons new lat))
        (else (cons (car lat) ((insertL-f test?) old new (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (old new lat)
      (cond
        ((null? lat) '())
        ((test? old (car lat)) (cons old (cons new (cdr lat))
        (else (cons (car lat) ((insertR-f test?) old new (cdr lat))))))))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL-g
  (insert-g
    (lambda (new old l)
      (cons new (cons old l)))))

(define insertR-g
  (insert-g
    (lambda (new old l)
      (cons old (cons new l)))))

(define subst-g
  (insert-g
    (lambda (new old l)
      (cons new l))))

(define rember-g
  (insert-g
    (lambda (new old l)
      l)))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x 'x) *)
      ((eq? x '^) oexp))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new (cons old (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons old (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else
        (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&col
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&col new oldL oldR (cdr lat)
                      (lambda (newlat L R)
                        (col (cons new (cons (car lat) newlat)) (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&col new oldL oldR (cdr lat)
                      (lambda (newlat L R)
                        (col (cons (car lat) (cons new newlat)) L (add1 R)))))
      (else
        (multiinsertLR&col new oldL oldR (cdr lat)
                          (lambda (newlat L R)
                            (col (cons (car lat) newlat) L R)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? a (car lat))
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col newlat (cons (car lat) seen)))))
      (else
        (multirember&co a (cdr lat)
                        (lambda (newlat seen)
                          (col (cons (car lat) newlat) seen)))))))

(define a-friend (lambda (x y) (null? y)))

(define new-friend (lambda (newlat seen) (a-friend newlat (cons 'tuna seen))))

(define print-results (lambda (newlat L R)
                        (cons newlat (cons L (cons R '())))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (evens-only* (car l)) (evens-only* (cdr l))))
      ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
      (else (evens-only* (cdr l))))))

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl m s)
                            (col (cons (car l) newl) (* m (car l)) s))))
         (else
          (evens-only*&co (cdr l)
                          (lambda (newl m s)
                            (col newl m (+ (car l) s)))))))
      (else
        (evens-only*&co (car l)
                        (lambda (newl1 m1 s1)
                          (evens-only*&co (cdr l)
                                          (lambda (newl2 m2 s2)
                                            (col (cons newl1 newl2) (* m1 m2) (+ s1 s2))))))))))

(define keep-looking
  (lambda (a p lat)
    (cond
      ((number? p) (keep-looking a (pick p lat) lat))
      (else (eq? a p)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else
        (build (first pora)
               (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
        (+ (length* (first pora))
           (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
        (+ (* (weight* (first pora)) 2)
           (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))

(define new-entry build)

(define lookup-in-entry-help
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car vals))
      (else (lookup-in-entry-help name (cdr names) (cdr vals) entry-f)))))


(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t)  *const)
      ((eq? e #f)  *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         else *application))
      (else *application))))

(define value
  (lambda (e)
    (meaning e ('()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table (lambda (name) (cons name '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))
