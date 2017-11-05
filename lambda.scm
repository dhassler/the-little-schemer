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


(evens-only*&co '(10 4 7 1 2 (3 4 8 9 1 2) 5 10 20) print-results)
