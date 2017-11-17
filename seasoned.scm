
(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? a (car lat))))))


(define sum-of-prefixes-b
  (lambda (total tup)
    (cond
      ((null? tup) '())
      (else (cons (+ total (car tup))
                  (sum-of-prefixes-b
                    (+ total (car tup))
                    (cdr tup)))))))

(define sum-of-prefixes
  (lambda (tup)
      (sum-of-prefixes-b 0 tup)))

(define sub1 (lambda (n) (- n 1)))

(define pick
  (lambda (n lat)
    (cond
      ((eq? 1 n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define scramble-b
  (lambda (tup revpre)
    (cond
      ((null? tup) '())
      (else
        (cons (pick (car tup)
                    (cons (car tup) revpre))
              (scramble-b (cdr tup) (cons (car tup) revpre)))))))

(define scramble
  (lambda (tup) (scramble-b tup '())))

(define multirember
  (lambda (a lat)
    ((letrec
       ((mr (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? a (car lat)) (mr (cdr lat)))
                (else (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))

(define rember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat)) (cdr lat))
        (else (cons (car lat) ((rember-f test?) a (cdr lat))))))))

(define rember (rember-f eq?))

(define multirember-f
  (lambda (test?)
    (letrec
       ((m-f (lambda (a lat)
              (cond
                ((null? lat) '())
                ((test? a (car lat)) (m-f a (cdr lat)))
                (else (cons (car lat) (m-f a (cdr lat))))))))
       m-f)))

(define member?
  (lambda (a lat)
    (letrec ((yes? (lambda (l)
              (cond
                ((null? l) #f)
                ((eq? (car l) a) #t)
                (else (yes? (cdr l)))))))
      (yes? lat))))

(define union
  (lambda (lat1 lat2)
    (letrec ((U (lambda (l)
                  (cond
                    ((null? l) lat2)
                    ((member? (car l) lat2) (union (cdr l) lat2))
                    (else (union (cdr l) (cons (car l) lat2)))))))
      (U lat1))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))

(define two-in-a-row-b?
  (letrec ((W
    (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      ((eq? preceding (car lat)) #t)
      (else (W (car lat) (cdr lat)))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

(define intersect
  (lambda (set1 set2)
    (letrec ((I
               (lambda (s1)
                 (cond
                   ((null? s1) '())
                   ((member? (car s1) set2)
                    (cons (car s1) (I (cdr s1))))
                   (else (I (cdr s1)))))))
      (I set1))))

(define-syntax letcc
  (syntax-rules ()
                ((letcc var body ...)
                 (call-with-current-continuation
                   (lambda (var)  body ... )))))

(define-syntax try
  (syntax-rules ()
                ((try var a . b)
                 (letcc success
                        (letcc var (success a)) . b))))

(define intersectall
  (lambda (l)
    (letcc hop
           (letrec ((I (lambda (lset)
                         (cond
                           ((null? (car lset)) (hop '()))
                           ((null? (cdr lset)) (car lset))
                           (else
                             (intersect (car lset) (I (cdr lset))))))))
             (cond
               ((null? l) '())
               (else (I l)))))))
